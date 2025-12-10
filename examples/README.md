# Hybrid_Lib_Ada Examples

This directory contains working examples demonstrating the Hybrid_Lib_Ada library's capabilities.

## Building Examples

```bash
# Build all examples
alr exec -- gprbuild -P examples/examples.gpr

# Or from the examples directory
cd examples
alr exec -- gprbuild -P examples.gpr
```

Executables will be placed in `examples/bin/`.

## Running Examples

### 1. basic_greeting - Simple Library Usage

```bash
./examples/bin/basic_greeting
```

Demonstrates the most basic usage:
- Create a greet command with a name
- Execute via the API facade
- Handle the Result

### 2. error_handling - Result Monad Pattern

```bash
./examples/bin/error_handling
```

Demonstrates railway-oriented error handling:
- Valid name succeeds
- Empty name returns validation error
- Name too long returns validation error

## Key Features Demonstrated

- **Result Monad Pattern**: All operations return `Result[T, Error]`
- **NO EXCEPTIONS**: Functional error handling with explicit error paths
- **Hybrid_Lib_Ada.API Facade**: Public API for library operations
- **Railway-Oriented Programming**: Chained Result operations

## Example Structure

Each example follows this pattern:

```ada
with Hybrid_Lib_Ada.API;
with Domain.Error.Result;

procedure Example is
   use Hybrid_Lib_Ada.API;

   --  Create command
   Cmd : constant Greet_Command := Create_Greet_Command ("Name");

   --  Execute - returns Result[Unit]
   Result : constant Unit_Result.Result := Greet (Cmd);
begin
   if Unit_Result.Is_Ok (Result) then
      --  Success path
   else
      --  Error path - inspect Error_Info
      declare
         Err : constant Error_Type := Unit_Result.Error_Info (Result);
      begin
         --  Handle error
      end;
   end if;
end Example;
```

## See Also

- [Main README](../README.md) - Library overview and installation
- [Software Requirements Specification](../docs/formal/software_requirements_specification.md)
- [Software Design Specification](../docs/formal/software_design_specification.md)
- [Test Suite](../test/) - Comprehensive usage examples

## License

Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [../LICENSE](../LICENSE) for details.
