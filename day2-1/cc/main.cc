#include <iostream>
#include <memory>
#include <optional>
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
  for (int pc = 0; pc >= 0; pc = run_instruction(memory, pc));
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

void supply_inputs(ProgramMemory* memory, int noun, int verb) {
  (*memory)[1] = noun;
  (*memory)[2] = verb;
}

void restore_1202_program_alarm_state(ProgramMemory* memory) {
  supply_inputs(memory, 12, 2);
}

std::optional<std::pair<int, int>>
search_for_output(const ProgramMemory& memory_template,
                  int expected_output) {
  for (int noun = 0; noun < 1000; ++noun) {
    for (int verb = 0; verb < 1000; ++verb) {
      ProgramMemory memory = memory_template;
      supply_inputs(&memory, noun, verb);
      run_program(&memory);
      if (memory.front() == expected_output) {
        return std::make_optional(std::pair<int, int>{noun, verb});
      }
    }
  }

  return std::nullopt;
}

int main(int argc, char *argv[]) {
  auto memory = initialize_memory();
  auto inputs = search_for_output(*memory, 19690720);
  if (!inputs.has_value()) {
    std::cout << "Valid inputs not found." << std::endl;
    return 1;
  }

  int noun, verb;
  std::tie(noun, verb) = inputs.value();
  std::cout << "noun = " << noun << ", verb = " << verb << std::endl;

  return 0;
}
