#include <iomanip>
#include <iostream>
#include <fstream>
#include <limits>
#include <memory>
#include <optional>
#include <vector>

enum class Opcode : int {
  ADD = 1,
  MULTIPLY = 2,
  SCAN = 3,
  PRINT = 4,
  HALT = 99
};

enum class ParameterMode : int {
  POSITION = 0,
  IMMEDIATE = 1
};

using ProgramMemory = std::vector<int>;
using ProgramCounter = int;

int get_parameter(const ProgramMemory* memory, const ParameterMode mode, const int loc_or_value) {
  switch (mode) {
    case ParameterMode::POSITION:
      return (*memory)[loc_or_value];
    case ParameterMode::IMMEDIATE:
      return loc_or_value;
  }
}

class Operation {
 public:
  Operation(int raw_instruction) : raw_instruction_(raw_instruction) {
    opcode_ = static_cast<Opcode>(raw_instruction % 100);
    a_mode_ = static_cast<ParameterMode>(
        (raw_instruction / 100) % 10);
    b_mode_ = static_cast<ParameterMode>(
        (raw_instruction / 1000) % 10);
    c_mode_ = static_cast<ParameterMode>(
        (raw_instruction / 10000) % 10);
  }

  Opcode opcode() const {
    return opcode_;
  }

  int a_value(const ProgramMemory* memory, const ProgramCounter pc) const {
    return get_parameter(memory, a_mode_, (*memory)[pc+1]);
  }

  int b_value(const ProgramMemory* memory, const ProgramCounter pc) const {
    return get_parameter(memory, b_mode_, (*memory)[pc+2]);
  }

  int c_value(const ProgramMemory* memory, const ProgramCounter pc) const {
    return get_parameter(memory, c_mode_, (*memory)[pc+3]);
  }

 private:
  friend std::ostream& operator<<(std::ostream& os, const Operation& op);

  int raw_instruction_;
  Opcode opcode_;
  ParameterMode a_mode_;
  ParameterMode b_mode_;
  ParameterMode c_mode_;
};

std::ostream& operator<<(std::ostream& os, const Operation& op) {
  char curr_fill = os.fill();
  os << std::setfill('0')
     << "[inst:" << std::setw(5) << op.raw_instruction_
     << " opcode:" << std::setw(2) << static_cast<int>(op.opcode_)
     << " a_mode:" << static_cast<int>(op.a_mode_)
     << " b_mode:" << static_cast<int>(op.b_mode_)
     << " c_mode:" << static_cast<int>(op.c_mode_)
     << "]"
     << std::setfill(curr_fill);
  return os;
}

namespace instructions {

ProgramCounter add(const Operation& op, ProgramMemory* memory, ProgramCounter pc) {
  int a = op.a_value(memory, pc);
  int b = op.b_value(memory, pc);
  int rc = (*memory)[pc+3];

  (*memory)[rc] = a + b;

  return pc + 4;
}

ProgramCounter multiply(const Operation& op, ProgramMemory* memory, ProgramCounter pc) {
  int a = op.a_value(memory, pc);
  int b = op.b_value(memory, pc);
  int rc = (*memory)[pc+3];

  (*memory)[rc] = a * b;

  return pc + 4;
}

ProgramCounter scan(const Operation& op, ProgramMemory* memory, ProgramCounter pc) {
  int ra = (*memory)[pc+1];

  int in;
  std::cout << "Scan? ";
  std::cin >> in;

  (*memory)[ra] = in;

  return pc + 2;
}

ProgramCounter print(const Operation& op, ProgramMemory* memory, ProgramCounter pc) {
  int a = op.a_value(memory, pc);
  std::cout << "Print: " << a << std::endl;

  return pc + 2;
}

ProgramCounter halt(const Operation&, ProgramMemory*, ProgramCounter) {
  return -1;
}

}  // namespace instructions

// Returns the new value of the program counter.
int run_instruction(ProgramMemory* memory, int pc) {
  using namespace ::instructions;

  Operation op((*memory)[pc]);
  switch (op.opcode()) {
    case Opcode::ADD:
      return add(op, memory, pc);
    case Opcode::MULTIPLY:
      return multiply(op, memory, pc);
    case Opcode::SCAN:
      return scan(op, memory, pc);
    case Opcode::PRINT:
      return print(op, memory, pc);
    case Opcode::HALT:
      return halt(op, memory, pc);
    default:
      std::cerr << "Unexpected operation: " << op << std::endl;
      return -1;
  }
}

void run_program(ProgramMemory* memory) {
  for (int pc = 0; pc >= 0; pc = run_instruction(memory, pc));
}

std::unique_ptr<ProgramMemory> initialize_memory(const char *input_file) {
  auto memory = std::make_unique<ProgramMemory>();
  std::ifstream ms(input_file);

  int x;
  while (!ms.eof()) {
    ms >> x;
    if (!std::cin.eof()) {
      memory->push_back(x);
      ms.ignore(std::numeric_limits<std::streamsize>::max(), ',');
    }
  }

  return memory;
}

void print_memory(const ProgramMemory& memory) {
  int i = 0;
  for (; i < memory.size() - 1; ++i) {
    std::cout << memory[i] << ",";
  }
  std::cout << memory[i] << std::endl;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    return 1;
  }

  auto memory = initialize_memory(argv[1]);
  run_program(memory.get());

  return 0;
}
