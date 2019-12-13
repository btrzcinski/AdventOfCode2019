#include <iostream>
#include <memory>
#include <vector>

enum class Opcode : int {
  ADD = 1,
  MULTIPLY = 2,
  HALT = 99
};

using ProgramMemory = std::vector<int>;

namespace instructions {

// Returns the new value of the program counter.
int add(ProgramMemory* memory, int pc) {
  int ra = (*memory)[pc+1];
  int rb = (*memory)[pc+2];
  int rc = (*memory)[pc+3];

  (*memory)[rc] = (*memory)[ra] + (*memory)[rb];

  return pc + 4;
}

// Returns the new value of the program counter.
int multiply(ProgramMemory* memory, int pc) {
  int ra = (*memory)[pc+1];
  int rb = (*memory)[pc+2];
  int rc = (*memory)[pc+3];

  (*memory)[rc] = (*memory)[ra] * (*memory)[rb];

  return pc + 4;
}

// Returns the new value of the program counter.
int halt(ProgramMemory*, int) {
  return -1;
}

}  // namespace instructions

// Returns the new value of the program counter.
int run_instruction(ProgramMemory* memory, int pc) {
  using namespace ::instructions;

  Opcode instruction = static_cast<Opcode>((*memory)[pc]);
  switch (instruction) {
    case Opcode::ADD:
      return add(memory, pc);
    case Opcode::MULTIPLY:
      return multiply(memory, pc);
    case Opcode::HALT:
      return halt(memory, pc);
    default:
      std::cerr << "Unexpected opcode: "
                << static_cast<int>(instruction)
                << std::endl;
      return -1;
  }
}

void run_program(ProgramMemory* memory) {
  int pc = 0;
  for (;;) {
    pc = run_instruction(memory, pc);
    if (pc < 0) break;
  }
}

std::unique_ptr<ProgramMemory> initialize_memory() {
  auto memory = std::make_unique<ProgramMemory>();

  int x;
  while (!std::cin.eof()) {
    std::cin >> x;
    if (!std::cin.eof()) {
      memory->push_back(x);
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

void restore_1202_program_alarm_state(ProgramMemory* memory) {
  (*memory)[1] = 12;
  (*memory)[2] = 2;
}

int main(int argc, char *argv[]) {
  auto memory = initialize_memory();
  restore_1202_program_alarm_state(memory.get());
  run_program(memory.get());
  print_memory(*memory);

  return 0;
}
