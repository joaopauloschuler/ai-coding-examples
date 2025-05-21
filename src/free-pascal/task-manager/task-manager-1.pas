
program AdvancedTaskManager;

{$mode objfpc} // Use Object Pascal mode for dynamic arrays and objects

uses
  SysUtils, // For basic system utilities, string manipulation (Trim, Copy, Pos, LowerCase, Val, Format), and file handling (if needed), AnsiCompareText
  Classes;  // Not strictly needed for dynamic arrays but might be useful later, keep it for compatibility/future

type
  TStringArray = array of string; // Type for command arguments

// --- Data Structures ---

type
  TTaskStatus = (tsPending, tsCompleted); // Enum for task status

  TTask = record
    ID: Integer;          // Unique identifier for the task
    Description: string;  // Task description
    Status: TTaskStatus;  // Current status of the task
  end;

  TTaskList = array of TTask; // Dynamic array to hold tasks

// --- Global Variables ---
var
  Tasks: TTaskList;       // The list of tasks
  NextTaskID: Integer = 1; // Counter for unique task IDs
  IsExit: Boolean = False; // Flag to control the main program loop

// --- Persistence Constants ---
const
  TASK_DATA_FILE = 'tasks.dat'; // Default filename for task data
  DATA_DELIMITER = '|';       // Delimiter for fields in the data file

// --- String Escaping/Unescaping for Persistence (from Solution 2) ---
// These functions handle escaping the delimiter '|' and the escape character '\'
// within task descriptions to prevent parsing errors during file load.

// Function to escape delimiter and escape characters in a string
function EscapeString(const InputString: string): string;
var
  I: Integer;
  OutputString: string;
begin
  OutputString := ''; // Initialize empty string
  for I := 1 to Length(InputString) do
  begin
    case InputString[I] of
      '\': OutputString := OutputString + '\\'; // Escape the escape character '\' -> '\\'
      '|': OutputString := OutputString + '\|'; // Escape the delimiter '|' -> '\|'
      else OutputString := OutputString + InputString[I]; // Keep other characters
    end;
  end;
  Result := OutputString;
end;

// Function to unescape delimiter and escape characters in a string
function UnescapeString(const InputString: string): string;
var
  I: Integer;
  OutputString: string;
begin
  OutputString := ''; // Initialize empty string
  I := 1;
  while I <= Length(InputString) do
  begin
    if InputString[I] = '\' then // Found potential escape character
    begin
      if I + 1 <= Length(InputString) then
      begin
        case InputString[I+1] of
          '\': begin OutputString := OutputString + '\'; I := I + 2; end; // Unescape '\\' -> '\'
          '|': begin OutputString := OutputString + '|'; I := I + 2; end; // Unescape '\|' -> '|'
          else // Unrecognized escape sequence (e.g., '\x'), treat '\' as literal
            begin
              OutputString := OutputString + InputString[I]; // Add '\'
              I := I + 1; // Move to next char (after '\')
            end;
        end;
      end
      else // '\' is the last character, treat as literal
      begin
        OutputString := OutputString + InputString[I]; // Add '\'
        I := I + 1;
      end;
    end
    else // Not an escape character
    begin
      OutputString := OutputString + InputString[I];
      I := I + 1;
    end;
  end;
  Result := OutputString;
end;


// Procedure to parse arguments from a string (from Solution 1)
// Handles spaces and simple single quotes. Does NOT handle escaped quotes.
procedure ParseArguments(const InputString: string; var ArgList: TStringArray);
var
  I: Integer;
  StartPos: Integer;
  InQuote: Boolean;
  CurrentChar: Char;
  CurrentArg: string;
  TrimmedInput: string;
begin
  SetLength(ArgList, 0); // Start with an empty list
  TrimmedInput := Trim(InputString);
  if TrimmedInput = '' then Exit; // No arguments

  I := 1;
  InQuote := False;
  StartPos := I; // Start of the current argument attempt

  while I <= Length(TrimmedInput) do
  begin
    CurrentChar := TrimmedInput[I];

    if InQuote then
    begin
      // Inside a quote, look for the closing quote (single quote)
      if CurrentChar = '''' then // Found closing quote
      begin
        // The content is from StartPos up to I-1
        CurrentArg := Copy(TrimmedInput, StartPos, I - StartPos);
        SetLength(ArgList, Length(ArgList) + 1);
        ArgList[High(ArgList)] := CurrentArg;

        InQuote := False;
        Inc(I); // Move past the closing quote
        // After a quote, skip any following spaces until the next argument starts
        while (I <= Length(TrimmedInput)) and (TrimmedInput[I] = ' ') do
          Inc(I);
        StartPos := I; // The next argument starts here
      end
      else
      begin
        // Still inside the quote, part of the current argument
        Inc(I);
      end;
    end
    else // Not in a quote, looking for space or opening quote
    begin
      if CurrentChar = '''' then // Found opening quote
      begin
        // Start of a quoted argument. Add any preceding non-quoted text as a separate argument first.
        if I > StartPos then // If there was content before the quote
        begin
           CurrentArg := Copy(TrimmedInput, StartPos, I - StartPos);
           SetLength(ArgList, Length(ArgList) + 1);
           ArgList[High(ArgList)] := CurrentArg;
           // Skip spaces after the processed argument
           while (I <= Length(TrimmedInput)) and (TrimmedInput[I] = ' ') do
             Inc(I);
           StartPos := I; // Update start position before processing the quoted part
        end;

        InQuote := True;
        StartPos := I + 1; // Argument content starts *after* the quote
        Inc(I); // Move past the opening quote
      end
      else if CurrentChar = ' ' then // Found space outside quote
      begin
        // Found end of an unquoted argument
        if I >= StartPos then // Ensure there's content for the argument
        begin
          CurrentArg := Copy(TrimmedInput, StartPos, I - StartPos);
          SetLength(ArgList, Length(ArgList) + 1);
          ArgList[High(ArgList)] := CurrentArg;
        end;
        Inc(I); // Move past the space
        // Skip subsequent spaces
        while (I <= Length(TrimmedInput)) and (TrimmedInput[I] = ' ') do
          Inc(I);
        StartPos := I; // The next argument starts after these spaces
      end
      else
      begin
        // Part of an unquoted argument
        Inc(I);
      end;
    end; // else not InQuote

  end; // while I <= Length(TrimmedInput)

  // After the loop, handle any remaining characters as the last argument
  if StartPos <= Length(TrimmedInput) then
  begin
    if InQuote then
    begin
      // Error: Unclosed quote at the end of the string
      WriteLn('Error: Unclosed quote in arguments.');
      SetLength(ArgList, 0); // Return empty list on error indication
      Exit; // Exit the procedure on error
    end
    else
    begin
      // Add the final unquoted argument
      CurrentArg := Copy(TrimmedInput, StartPos, Length(TrimmedInput) - StartPos + 1);
      SetLength(ArgList, Length(ArgList) + 1);
      ArgList[High(ArgList)] := CurrentArg;
    end;
  end;

end;


// --- Core Task Procedures (Modified for Combined Features) ---

// Procedure to add a new task
procedure AddTask(const Description: string);
var
  NewTask: TTask;
  CurrentLength: Integer;
begin
  // Basic validation: check if description is empty
  if Trim(Description) = '' then
  begin
    WriteLn('Error: Task description cannot be empty.');
    Exit;
  end;

  // Create the new task record
  NewTask.ID := NextTaskID;       // Assign unique ID
  NewTask.Description := Description;
  NewTask.Status := tsPending;    // New tasks are initially pending

  // Increment the next ID counter for the next task
  Inc(NextTaskID);

  // Add the new task to the dynamic array
  CurrentLength := Length(Tasks);
  SetLength(Tasks, CurrentLength + 1); // Increase the size of the dynamic array
  Tasks[CurrentLength] := NewTask;     // Add the new task at the end

  WriteLn('Task added with ID: ', NewTask.ID);
end;

// Procedure to list tasks, with optional filtering
procedure ListTasks(const Filter: string = 'ALL');
var
  I: Integer;
  StatusStr: string;
  IncludeTask: Boolean;
  ActualFilter: string; // Local variable to process filter case-insensitivity
begin
  ActualFilter := UpperCase(Trim(Filter)); // Process filter once

  WriteLn('--- Task List (Filtered: ', ActualFilter, ') ---');
  if Length(Tasks) = 0 then
  begin
    WriteLn('No tasks added yet.');
  end
  else
  begin
    // Iterate through the dynamic array using High function
    for I := 0 to High(Tasks) do
    begin
      // Determine if task should be included based on filter
      IncludeTask := False;
      case ActualFilter of
        'ALL': IncludeTask := True;
        'PENDING': if Tasks[I].Status = tsPending then IncludeTask := True;
        'COMPLETED': if Tasks[I].Status = tsCompleted then IncludeTask := True;
        else // Default to show all if filter is invalid, and warn
          begin
            WriteLn('Warning: Invalid filter "', Filter, '". Showing all tasks.');
            IncludeTask := True;
          end;
      end;

      if IncludeTask then
      begin
        // Determine status string based on enum value
        case Tasks[I].Status of
          tsPending: StatusStr := 'Pending';
          tsCompleted: StatusStr := 'Completed';
        end;
        // Display task details including ID
        WriteLn(Format('ID: %d, Status: %s, Description: %s', [Tasks[I].ID, StatusStr, Tasks[I].Description]));
      end;
    end;
  end;
  WriteLn('-----------------');
end;

// Procedure to mark a task as completed by ID
procedure CompleteTask(const TaskID: Integer);
var
  I: Integer;
  Found: Boolean = False;
begin
  // Check if the list is empty before searching
  if Length(Tasks) = 0 then
  begin
     WriteLn('No tasks to complete.');
     Exit;
  end;

  // Iterate through tasks to find the one with the matching ID
  for I := 0 to High(Tasks) do
  begin
    if Tasks[I].ID = TaskID then
    begin
      // Found the task, mark it as completed
      Tasks[I].Status := tsCompleted;
      WriteLn('Task ID ', TaskID, ' marked as completed.');
      Found := True;
      Break; // Exit loop once found
    end;
  end;

  // If the task was not found, inform the user
  if not Found then
  begin
    WriteLn('Error: Task with ID ', TaskID, ' not found.');
  end;
end;

// Procedure to remove a task by ID
procedure RemoveTask(const TaskID: Integer);
var
  I, J: Integer;
  FoundIndex: Integer = -1; // Use -1 to indicate not found
begin
  // Check if the list is empty before searching
  if Length(Tasks) = 0 then
  begin
    WriteLn('No tasks to remove.');
    Exit;
  end;

  // Find the index of the task with the matching ID
  for I := 0 to High(Tasks) do
  begin
    if Tasks[I].ID = TaskID then
    begin
      FoundIndex := I; // Store the index
      Break; // Exit loop once found
    end;
  end;

  // If the task was found at FoundIndex
  if FoundIndex <> -1 then
  begin
    // Shift elements to fill the gap created by the removed task
    // Loop from the found index up to the element *before* the last one
    for J := FoundIndex to Length(Tasks) - 2 do
    begin
      Tasks[J] := Tasks[J+1];
    end;

    // Decrease the size of the dynamic array by 1
    SetLength(Tasks, Length(Tasks) - 1);

    WriteLn('Task with ID ', TaskID, ' removed.');
  end
  else
  begin
    // If the task was not found, inform the user
    WriteLn('Error: Task with ID ', TaskID, ' not found.');
  end;
end;

// Procedure to display details of a specific task by ID
procedure ViewTask(const TaskID: Integer);
var
  I: Integer;
  Found: Boolean = False;
  StatusStr: string;
begin
  WriteLn('--- Viewing Task ID: ', TaskID, ' ---');
  // Check if the list is empty before searching
  if Length(Tasks) = 0 then
  begin
     WriteLn('No tasks to view.');
     Exit;
  end;

  // Iterate through tasks to find the one with the matching ID
  for I := 0 to High(Tasks) do
  begin
    if Tasks[I].ID = TaskID then
    begin
      // Found the task, display details
      case Tasks[I].Status of
        tsPending: StatusStr := 'Pending';
        tsCompleted: StatusStr := 'Completed';
      end;
      WriteLn(Format('ID: %d, Status: %s, Description: %s', [Tasks[I].ID, StatusStr, Tasks[I].Description]));
      Found := True;
      Break; // Exit loop once found
    end;
  end;

  // If the task was not found, inform the user
  if not Found then
  begin
    WriteLn('Error: Task with ID ', TaskID, ' not found.');
  end;
  WriteLn('--------------------');
end;

// Procedure to sort tasks based on criteria
procedure SortTasks(const Criteria: string);
var
  I, J: Integer;
  TempTask: TTask;
  Sorted: Boolean;
  CompareResult: Integer; // For string comparison result
  ActualCriteria: string; // Local variable to process criteria case-insensitivity
begin
  ActualCriteria := UpperCase(Trim(Criteria)); // Process criteria once
  WriteLn('Sorting tasks by: ', ActualCriteria);

  if Length(Tasks) <= 1 then // No need to sort if 0 or 1 tasks
  begin
    WriteLn('Not enough tasks to sort.');
    Exit;
  end;

  // Use a simple Bubble Sort (can be optimized later if needed)
  Sorted := False; // Flag to optimize: if no swaps happen, list is sorted
  while not Sorted do
  begin
    Sorted := True; // Assume sorted until a swap occurs
    for I := 0 to High(Tasks) - 1 do
    begin
      // Assume I is the current task, I+1 is the next task
      case ActualCriteria of
        'ID':
          begin
            if Tasks[I].ID > Tasks[I+1].ID then
            begin
              TempTask := Tasks[I];
              Tasks[I] := Tasks[I+1];
              Tasks[I+1] := TempTask;
              Sorted := False;
            end;
          end;
        'STATUS':
          begin
            // Comparing enum values directly works (Pending < Completed)
            if Tasks[I].Status > Tasks[I+1].Status then
            begin
              TempTask := Tasks[I];
              Tasks[I] := Tasks[I+1];
              Tasks[I+1] := TempTask;
              Sorted := False;
            end;
          end;
        'DESCRIPTION':
          begin
            // Use AnsiCompareText for case-insensitive comparison
            CompareResult := AnsiCompareText(Tasks[I].Description, Tasks[I+1].Description);
            if CompareResult > 0 then // If Tasks[I].Description comes after Tasks[I+1].Description
            begin
              TempTask := Tasks[I];
              Tasks[I] := Tasks[I+1];
              Tasks[I+1] := TempTask;
              Sorted := False;
            end;
          end;
        else
          begin
            WriteLn('Error: Invalid sort criteria: ', Criteria, '. Use ID, Status, or Description.');
            Exit; // Exit the sort procedure if criteria is invalid
          end;
      end; // case ActualCriteria
    end; // for
  end; // while
  WriteLn('Sorting complete.');
end;

// Procedure to save tasks to a file (using EscapeString from Solution 2)
procedure SaveTasksToFile(const Filename: string);
var
  DataFile: TextFile;
  I: Integer;
  StatusInt: Integer;
begin
  AssignFile(DataFile, Filename);
  // Use Rewrite to create or overwrite the file
  {$I-} // Disable I/O checking temporarily
  Rewrite(DataFile);
  {$I+} // Re-enable I/O checking
  if IOResult <> 0 then
  begin
    WriteLn('Error: Could not write to file"', Filename, '".');
    Exit;
  end;

  // Write each task
  for I := 0 to High(Tasks) do
  begin
    // Convert status enum to integer (0 for Pending, 1 for Completed)
    case Tasks[I].Status of
      tsPending: StatusInt := 0;
      tsCompleted: StatusInt := 1;
    end;
    // ESCAPE the description before writing
    WriteLn(DataFile, Tasks[I].ID, DATA_DELIMITER, EscapeString(Tasks[I].Description), DATA_DELIMITER, StatusInt);
  end;

  CloseFile(DataFile);
  WriteLn('Tasks saved to "', Filename, '".');
end;

// Procedure to load tasks from a file (using UnescapeString from Solution 2)
procedure LoadTasksFromFile(const Filename: string);
var
  DataFile: TextFile;
  Line: string;
  Parts: array[0..2] of string; // Array to hold parts: ID, Description, Status
  CurrentTask: TTask;
  TaskID, StatusInt, ErrorCode: Integer;
  MaxLoadedID: Integer = 0; // To track the highest loaded ID
  P1, P2: Integer; // Positions of delimiters
begin
  // Clear current tasks before loading
  SetLength(Tasks, 0);

  AssignFile(DataFile, Filename);
  {$I-} // Disable I/O checking temporarily
  Reset(DataFile); // Open existing file for reading
  {$I+} // Re-enable I/O checking

  if IOResult <> 0 then
  begin
    // File not found or cannot be opened is not necessarily an error on startup
    WriteLn('Info: Could not open task data file"', Filename, '". Starting with an empty list.');
    Exit; // Exit if file does not exist or cannot be opened
  end;

  WriteLn('Loading tasks from "', Filename, '"...');

  while not Eof(DataFile) do
  begin
    ReadLn(DataFile, Line);
    // Basic validation: ignore empty lines
    if Trim(Line) = '' then Continue;

    // Parse the line using the delimiter '|'
    P1 := Pos(DATA_DELIMITER, Line);
    if P1 > 0 then
    begin
      P2 := Pos(DATA_DELIMITER, Line, P1 + 1); // Find second delimiter after the first one
      if P2 > 0 then
      begin
        // Extract parts: ID, Description, Status
        Parts[0] := Copy(Line, 1, P1 - 1);              // Part before first |
        Parts[1] := Copy(Line, P1 + 1, P2 - P1 - 1);    // Part between first and second |
        Parts[2] := Copy(Line, P2 + 1, Length(Line) - P2); // Part after second |

        // Validate and parse parts
        // Part 0: ID (must be integer)
        Val(Trim(Parts[0]), TaskID, ErrorCode);
        if ErrorCode <> 0 then
        begin
          WriteLn('Warning: Skipping malformed line (invalid Task ID: "', Parts[0], '"): "', Line, '"');
          Continue; // Skip this line
        end;

        // Part 2: Status (must be 0 or 1)
        Val(Trim(Parts[2]), StatusInt, ErrorCode);
        if (ErrorCode <> 0) or not ((StatusInt = 0) or (StatusInt = 1)) then
        begin
          WriteLn('Warning: Skipping malformed line (invalid Status: "', Parts[2], '"): "', Line, '"');
          Continue; // Skip this line
        end;

        // Part 1: Description (UNESCAPE special characters)
        CurrentTask.Description := UnescapeString(Parts[1]); // No need to trim if saving didn't trim

        // Create and add the task record to the dynamic array
        CurrentTask.ID := TaskID;
        case StatusInt of
          0: CurrentTask.Status := tsPending;
          1: CurrentTask.Status := tsCompleted;
        end;

        // Add to dynamic array
        SetLength(Tasks, Length(Tasks) + 1);
        Tasks[High(Tasks)] := CurrentTask; // Add to the end

        // Update MaxLoadedID
        if TaskID > MaxLoadedID then
          MaxLoadedID := TaskID;
      end
      else
      begin
         // Error: Only one delimiter found
         WriteLn('Warning: Skipping malformed line (missing second delimiter): "', Line, '"');
         Continue; // Skip this line
      end;
    end
    else
    begin
      // Error: First delimiter not found
      WriteLn('Warning: Skipping malformed line (missing first delimiter): "', Line, '"');
      Continue; // Skip this line
    end;

  end; // while not Eof

  CloseFile(DataFile);

  // Set the NextTaskID to be one greater than the maximum ID loaded
  // This prevents ID conflicts with loaded tasks
  if MaxLoadedID >= NextTaskID then
    NextTaskID := MaxLoadedID + 1;

  WriteLn('Loading complete.', Length(Tasks), ' tasks loaded.');
end;


// Procedure for Command-Based Input Handling (from Solution 1, adapted)
procedure ProcessCommand(const CommandLine: string);
var
  TrimmedLine, Command, ArgsString: string;
  SpacePos: Integer;
  TaskIDArg: Integer;
  ErrorCode: Integer;
  ArgList: TStringArray; // Using the dynamic array for parsed arguments

begin // Start of ProcessCommand logic
  TrimmedLine := Trim(CommandLine);
  if TrimmedLine = '' then Exit; // Ignore empty lines

  // Extract the command (the first word, case-insensitive)
  SpacePos := Pos(' ', TrimmedLine);

  if SpacePos > 0 then
  begin
    Command := Copy(TrimmedLine, 1, SpacePos - 1);
    // The rest is the argument string to be parsed
    ArgsString := Trim(Copy(TrimmedLine, SpacePos + 1, Length(TrimmedLine) - SpacePos));
  end
  else
  begin
    Command := TrimmedLine;
    ArgsString := ''; // No arguments string
  end;

  // Convert command to lowercase for case-insensitivity
  Command := LowerCase(Command);

  // Call the argument parsing procedure
  ParseArguments(ArgsString, ArgList);

  // --- Dispatch based on the command and validate arguments using ArgList ---
  case Command of
    'add':
      begin
        // The 'add' command expects exactly 1 argument (the description)
        if Length(ArgList) = 1 then
        begin
          AddTask(ArgList[0]); // Use the parsed argument as is, AddTask will trim
        end
        else if Length(ArgList) > 1 then
        begin
          WriteLn('Error: add command expects a single description argument, potentially quoted if it contains spaces.');
          WriteLn('Usage: add <description>');
        end
        else // Length(ArgList) = 0
        begin
          WriteLn('Error: add command requires a description. Usage: add <description>');
        end;
      end;
    'list':
      begin
        // The 'list' command takes an optional filter argument (ALL, PENDING, COMPLETED)
        if Length(ArgList) = 0 then
        begin
           ListTasks('ALL'); // Default to 'ALL' if no argument
        end
        else if Length(ArgList) = 1 then
        begin
           ListTasks(Trim(ArgList[0])); // Use the first argument as the filter
        end
        else // Length(ArgList) > 1
        begin
           WriteLn('Error: list command expects at most one filter argument (All, Pending, Completed). Usage: list [filter]');
        end;
      end;
    'complete':
      begin
        // The 'complete' command expects exactly 1 argument (the Task ID)
        if Length(ArgList) = 1 then
        begin
          Val(Trim(ArgList[0]), TaskIDArg, ErrorCode); // Attempt to parse the trimmed first argument as integer
          if ErrorCode <> 0 then
          begin
            WriteLn('Error: Invalid Task ID format. complete command requires a single valid integer ID.');
            WriteLn('Usage: complete <TaskID>');
          end
          else
          begin
            CompleteTask(TaskIDArg);
          end;
        end
        else if Length(ArgList) > 1 then
        begin
          WriteLn('Error: complete command expects a single Task ID.');
          WriteLn('Usage: complete <TaskID>');
        end
        else // Length(ArgList) = 0
        begin
          WriteLn('Error: complete command requires a Task ID. Usage: complete <TaskID>');
        end;
      end;
     'remove':
       begin
        // The 'remove' command expects exactly 1 argument (the Task ID)
        if Length(ArgList) = 1 then
        begin
          Val(Trim(ArgList[0]), TaskIDArg, ErrorCode); // Attempt to parse the trimmed first argument as integer
          if ErrorCode <> 0 then
          begin
            WriteLn('Error: Invalid Task ID format. remove command requires a single valid integer ID.');
            WriteLn('Usage: remove <TaskID>');
          end
          else
          begin
            RemoveTask(TaskIDArg);
          end;
        end
        else if Length(ArgList) > 1 then
        begin
          WriteLn('Error: remove command expects a single Task ID.');
          WriteLn('Usage: remove <TaskID>');
        end
        else // Length(ArgList) = 0
        begin
          WriteLn('Error: remove command requires a Task ID. Usage: remove <TaskID>');
        end;
       end;
     'view':
       begin
        // The 'view' command expects exactly 1 argument (the Task ID)
        if Length(ArgList) = 1 then
        begin
          Val(Trim(ArgList[0]), TaskIDArg, ErrorCode); // Attempt to parse the trimmed first argument as integer
          if ErrorCode <> 0 then
          begin
            WriteLn('Error: Invalid Task ID format. view command requires a single valid integer ID.');
            WriteLn('Usage: view <TaskID>');
          end
          else
          begin
            ViewTask(TaskIDArg);
          end;
        end
        else if Length(ArgList) > 1 then
        begin
          WriteLn('Error: view command expects a single Task ID.');
          WriteLn('Usage: view <TaskID>');
        end
        else // Length(ArgList) = 0
        begin
          WriteLn('Error: view command requires a Task ID. Usage: view <TaskID>');
        end;
       end;
     'sort': // New command
       begin
         // The 'sort' command expects exactly 1 argument (criteria: ID, Status, or Description)
         if Length(ArgList) = 1 then
         begin
           SortTasks(Trim(ArgList[0])); // Use the first argument as the sort criteria
         end
         else if Length(ArgList) > 1 then
         begin
           WriteLn('Error: sort command expects a single criteria (ID, Status, or Description).');
           WriteLn('Usage: sort <criteria>');
         end
         else // Length(ArgList) = 0
         begin
           WriteLn('Error: sort command requires a criteria (ID, Status, or Description). Usage: sort <criteria>');
         end;
       end;
     'filter': // Alias for list with filter
       begin
         // The 'filter' command expects exactly 1 argument (status: All, Pending, Completed)
         if Length(ArgList) = 1 then
         begin
           ListTasks(Trim(ArgList[0])); // Use the first argument as the filter
         end
         else if Length(ArgList) > 1 then
         begin
           WriteLn('Error: filter command expects a single status (All, Pending, Completed).');
           WriteLn('Usage: filter <status>');
         end
         else // Length(ArgList) = 0
         begin
           WriteLn('Error: filter command requires a status (All, Pending, Completed). Usage: filter <status>');
         end;
       end;
     'save': // Persistence command
       begin
         // The 'save' command takes an optional filename argument (0 or 1)
         if Length(ArgList) = 0 then
           SaveTasksToFile(TASK_DATA_FILE) // Use default filename
         else if Length(ArgList) = 1 then
         begin
           SaveTasksToFile(Trim(ArgList[0])) // Use the provided filename argument (trim it)
         end
         else // Length(ArgList) > 1
         begin
            WriteLn('Error: save command expects at most one filename argument.');
            WriteLn('Usage: save [filename]');
         end;
       end;
     'load': // Persistence command
       begin
         // The 'load' command takes an optional filename argument (0 or 1)
         if Length(ArgList) = 0 then
           LoadTasksFromFile(TASK_DATA_FILE) // Use default filename
         else if Length(ArgList) = 1 then
         begin
           LoadTasksFromFile(Trim(ArgList[0])) // Use the provided filename argument (trim it)
         end
         else // Length(ArgList) > 1
         begin
            WriteLn('Error: load command expects at most one filename argument.');
            WriteLn('Usage: load [filename]');
         end;
       end;
     'exit':
       begin
         // The 'exit' command takes no arguments, warn if provided
         if Length(ArgList) <> 0 then
         begin
            WriteLn('Warning: exit command takes no arguments. Ignoring arguments.');
         end;
         IsExit := True; // Set the global exit flag to true
         WriteLn('Exiting Task Manager.');
       end;
    'help':
      begin
        // The 'help' command takes no arguments, warn if provided
        if Length(ArgList) <> 0 then
        begin
           WriteLn('Warning: help command takes no arguments. Ignoring arguments.');
        end;
        // Display available commands and usage (Updated for all commands)
        WriteLn('Available commands:');
        WriteLn('  add <description> - Adds a new task. Description can be quoted if it contains spaces. Takes exactly 1 argument.');
        WriteLn('  list [filter]     - Lists tasks. Filter can be All, Pending, or Completed (default is All). Takes 0 or 1 argument.');
        WriteLn('  complete <TaskID> - Marks a task as completed by its ID. Takes exactly 1 integer argument.');
        WriteLn('  remove <TaskID>   - Removes a task by its ID. Takes exactly 1 integer argument.');
        WriteLn('  view <TaskID>     - Views details of a specific task by its ID. Takes exactly 1 integer argument.');
        WriteLn('  sort <criteria>   - Sorts tasks by ID, Status, or Description. Takes exactly 1 argument (ID, Status, or Description).');
        WriteLn('  filter <status>   - Alias for list command with filter. Takes exactly 1 argument (All, Pending, or Completed).');
        WriteLn('  save [filename]   - Saves tasks to a file (default: tasks.dat). Takes 0 or 1 argument.');
        WriteLn('  load [filename]   - Loads tasks from a file (default: tasks.dat). Takes 0 or 1 argument.');
        WriteLn('  help              - Displays this help message. Takes no arguments.');
        WriteLn('  exit              - Exits the task manager.');
      end;
    else
      begin
        // Handle unknown commands
        WriteLn('Error: Unknown command. Type "help" for a list of commands.');
      end;
  end; // end case Command
end; // end procedure ProcessCommand


// --- Main program execution block (Command-line interface) ---
var
  CommandLine: string; // Variable to store the full command line input

begin
  // Initialize the dynamic array to be empty and the exit flag
  SetLength(Tasks, 0);
  IsExit := False; // Ensure exit flag is false initially
  NextTaskID := 1; // Ensure ID counter starts from 1 on launch

  // Attempt to load tasks from file on startup
  LoadTasksFromFile(TASK_DATA_FILE);

  WriteLn('Advanced Task Manager started (Command Mode). Type "help" for commands.');
  WriteLn; // Add a newline for better formatting

  // Main loop for command-based user interaction
  repeat
    Write('> '); // Command prompt
    ReadLn(CommandLine); // Read the entire line of input

    ProcessCommand(CommandLine); // Call the procedure to process the command

    WriteLn; // Add a newline for better formatting after each command output

  until IsExit; // Loop continues until IsExit flag is true

  // Save tasks to file on exit
  SaveTasksToFile(TASK_DATA_FILE);

  // Explicitly set length to 0 before program termination is good practice
  SetLength(Tasks, 0); // Clean up dynamic array memory

end.
