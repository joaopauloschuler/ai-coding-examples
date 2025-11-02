# AI Coding Examples

This repository contains AI-generated source code examples created using [Beyond Python SmoLAgents](https://github.com/joaopauloschuler/beyond-python-smolagents).

## Purpose

The primary goals of this repository are:
- **Storage**: Preserve AI-generated code examples for reference and documentation
- **Comparison**: Enable future analysis and comparison of AI coding results across different:
  - Programming languages
  - AI models
  - Problem-solving approaches
  - Time periods

## Repository Structure

The repository organizes code examples by programming language:

```
src/
├── free-pascal/
│   └── task-manager/
├── php/
│   └── task-manager/
└── [other languages]/
```

Each example includes:
- Complete source code implementations
- Documentation and usage guides
- AI-generated metadata (where applicable)

## Compiling and Running Examples

### Free Pascal (Object Pascal)

To compile a Free Pascal source file, use the Free Pascal Compiler (fpc):

```bash
fpc source_file.pas -obin/output_name -O1 -Mobjfpc
```

**Example:**
```bash
fpc src/free-pascal/task-manager/task-manager-1.pas -obin/task-manager -O1 -Mobjfpc
```

Where:
- `source_file.pas` - The Pascal source file to compile
- `-obin/output_name` - Specifies the output binary location and name
- `-O1` - Enables level 1 optimizations
- `-Mobjfpc` - Sets the compiler mode to Object Pascal

After compilation, run the binary:
```bash
./bin/task-manager
```

## About the Code Generation

All source code in this repository was generated using the [Beyond Python SmoLAgents](https://github.com/joaopauloschuler/beyond-python-smolagents) framework, which demonstrates AI capabilities in software development across multiple programming languages.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contributing

This repository serves as an archive of AI-generated code examples. The examples are preserved in their generated state to maintain their value as historical references for comparison and analysis.

---

**Repository maintained by**: [@joaopauloschuler](https://github.com/joaopauloschuler)
