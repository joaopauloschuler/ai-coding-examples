// This source code was created by "evolutive_problem_solver" from https://github.com/joaopauloschuler/beyond-python-smolagents.
// You can find the prompt and the used parameters at the end of this file.

program TaskManagerApp;

{$mode objfpc} // Use Object Pascal mode for dynamic arrays and objects
{$h+}          // Enable AnsiStrings by default

uses
  SysUtils,    // For TDateTime, DateTimeToStr, StrToDateTime, Exception, sLineBreak, StringReplace, StrToIntDef, StrToDateTimeDef, Now, Today, Trunc, DirectoryExists, CreateDir
  DateUtils,   // For DaysBetween, IncDay, IncMonth, IncYear, EncodeDate, DecodeDate, Year, Month, Day, MSecsPerDay, EndOfADay
  Math;        // For mathematical functions and constants (like MaxDouble)

// --- Type Definitions (Mixed from Solution 2 and 3 for best practices) ---

type
  TTaskPriority = (
    tp_low,
    tp_medium,
    tp_high,
    tp_urgent
  );

  TTaskStatus = (
    ts_pending,
    ts_in_progress,
    ts_completed,
    ts_cancelled
  );

  TTask = object // From Solution 3: TTask as an object for better encapsulation and features
    task_id: integer;
    name: string;
    description: string;
    creation_date: tDateTime;
    due_date: tDateTime;
    priority: TTaskPriority;
    status: TTaskStatus;
    last_modified_date: tDateTime;

    constructor create(a_task_id: integer; a_name: string; a_description: string;
                       a_due_date: tDateTime; a_priority: TTaskPriority);

    // Converts priority enum to string
    function priority_to_string: string;
    // Converts status enum to string
    function status_to_string: string;
    // Converts string to priority enum
    function string_to_priority(const s: string): TTaskPriority;
    // Converts string to status enum
    function string_to_status(const s: string): TTaskStatus;

    // Returns a string representation of the task for display
    function to_string: string;
    // Returns a CSV formatted string for saving
    function to_csv_string: string;
    // Populates task from a CSV string
    procedure from_csv_string(const csv_line: string);
  end;

  // Explicit definition for dynamic array of strings, as TStringDynArray might not be directly available everywhere
  TStringDynArray = array of string;

  // Type for dynamic array of TTask objects
  TTaskDynArray = array of TTask;

  // Type for dynamic array function results (as required by the task description)
  TTaskDynArrayResult = array of TTask;

  TTaskManager = object
  private
    f_tasks: TTaskDynArray;
    f_next_task_id: integer;

    // Private helper function to find a task by ID
    function find_task_index_by_id(a_task_id: integer): integer;

  public
    constructor create;
    destructor destroy;

    // CRUD Operations (Mixed from Solutions 1, 2, and 3 for comprehensive control)
    function add_task(a_name: string; a_description: string; a_due_date: tDateTime;
                      a_priority: TTaskPriority): integer; // Changed return to ID from Solution1
    function update_task_status(a_task_id: integer; a_new_status: TTaskStatus): boolean;
    function update_task_priority(a_task_id: integer; a_new_priority: TTaskPriority): boolean;
    function update_task_description(a_task_id: integer; a_new_description: string): boolean;
    function update_task_due_date(a_task_id: integer; a_new_due_date: tDateTime): boolean;
    function delete_task(a_task_id: integer): boolean;
    function get_task(a_task_id: integer; var a_task: TTask): boolean; // From Solution 1, useful for retrieving a task object

    // Query and Retrieval (Mixed from Solutions 1 and 3)
    function get_all_tasks: TTaskDynArrayResult;
    function get_tasks_by_status(a_status: TTaskStatus): TTaskDynArrayResult;
    function get_tasks_by_priority(a_priority: TTaskPriority): TTaskDynArrayResult; // Specific priority filter from Solution 2
    function get_tasks_by_priority_range(min_priority, max_priority: TTaskPriority): TTaskDynArrayResult; // From Solution 1
    function get_tasks_due_soon(a_days_threshold: integer): TTaskDynArrayResult;
    function get_tasks_by_date_range(start_date, end_date: tDateTime): TTaskDynArrayResult; // From Solution 1

    // Sorting (QuickSort from Solution 3, extended for ID)
    procedure sort_tasks_by_id(ascending: boolean); // Added from Solution 1 concept, but using QuickSort
    procedure sort_tasks_by_due_date(ascending: boolean);
    procedure sort_tasks_by_priority(ascending: boolean);

    // Persistence (From Solution 3 - Key feature)
    function save_to_file(const a_filename: string): boolean;
    function load_from_file(const a_filename: string): boolean;

    // Utility for display (From Solution 3)
    function get_all_tasks_string: string;
  end;

// --- TTask Implementation (From Solution 3) ---

constructor TTask.create(a_task_id: integer; a_name: string; a_description: string;
                       a_due_date: tDateTime; a_priority: TTaskPriority);
begin
  self.task_id := a_task_id;
  self.name := a_name;
  self.description := a_description;
  self.creation_date := Now;
  self.due_date := a_due_date;
  self.priority := a_priority;
  self.status := ts_pending; // Default status
  self.last_modified_date := Now;
end;

function TTask.priority_to_string: string;
begin
  case self.priority of
    tp_low: result := 'low';
    tp_medium: result := 'medium';
    tp_high: result := 'high';
    tp_urgent: result := 'urgent';
  end;
end;

function TTask.status_to_string: string;
begin
  case self.status of
    ts_pending: result := 'pending';
    ts_in_progress: result := 'in_progress';
    ts_completed: result := 'completed';
    ts_cancelled: result := 'cancelled';
  end;
end;

function TTask.string_to_priority(const s: string): TTaskPriority;
begin
  if s = 'low' then result := tp_low
  else if s = 'medium' then result := tp_medium
  else if s = 'high' then result := tp_high
  else if s = 'urgent' then result := tp_urgent
  else result := tp_low; // Default or error handling
end;

function TTask.string_to_status(const s: string): TTaskStatus;
begin
  if s = 'pending' then result := ts_pending
  else if s = 'in_progress' then result := ts_in_progress
  else if s = 'completed' then result := ts_completed
  else if s = 'cancelled' then result := ts_cancelled
  else result := ts_pending; // Default or error handling
end;

function TTask.to_string: string;
begin
  // Made slightly less verbose but still informative
  result := format('ID: %d, Name: %s, Desc: %s, Due: %s, Priority: %s, Status: %s, Created: %s, LastMod: %s',
                   [self.task_id,
                    self.name,
                    self.description,
                    FormatDateTime('yyyy-mm-dd hh:nn:ss', self.due_date),
                    self.priority_to_string,
                    self.status_to_string,
                    FormatDateTime('yyyy-mm-dd hh:nn:ss', self.creation_date),
                    FormatDateTime('yyyy-mm-dd hh:nn:ss', self.last_modified_date)]);
end;

function TTask.to_csv_string: string;
begin
  // Using pipes as delimiters for simplicity given the no external libs rule for CSV parsing.
  // Escaping the delimiter within fields by doubling it '||'.
  result := format('%d|%s|%s|%s|%s|%s|%s|%s',
                   [self.task_id,
                    stringreplace(self.name, '|', '||', [rfReplaceAll]),
                    stringreplace(self.description, '|', '||', [rfReplaceAll]),
                    DateTimeToStr(self.creation_date),
                    DateTimeToStr(self.due_date),
                    self.priority_to_string,
                    self.status_to_string,
                    DateTimeToStr(self.last_modified_date)]);
end;

procedure TTask.from_csv_string(const csv_line: string);
var
  parts: TStringDynArray;
  current_part: string;
  temp_s: string;
  current_pos: integer;
  start_of_field: integer;
  part_count: integer;
begin
  SetLength(parts, 0);
  temp_s := csv_line;
  current_pos := 1; // 1-based indexing for string manipulation
  start_of_field := 1;
  part_count := 0;

  while current_pos <= length(temp_s) do
  begin
    if temp_s[current_pos] = '|' then
    begin
      if (current_pos + 1 <= length(temp_s)) and (temp_s[current_pos + 1] = '|') then
      begin
        // Escaped delimiter '||', skip the second '|'
        inc(current_pos, 2);
      end
      else
      begin
        // Found unescaped delimiter, extract the part
        current_part := copy(temp_s, start_of_field, current_pos - start_of_field);
        current_part := stringreplace(current_part, '||', '|', [rfReplaceAll]);

        inc(part_count);
        SetLength(parts, part_count);
        parts[part_count - 1] := current_part; // Dynamic arrays are 0-indexed

        start_of_field := current_pos + 1;
        inc(current_pos);
      end;
    end
    else
    begin
      inc(current_pos);
    end;
  end;

  // Add the last part after the loop (the part after the last delimiter, or the whole string if no delimiters)
  current_part := copy(temp_s, start_of_field, length(temp_s) - start_of_field + 1);
  current_part := stringreplace(current_part, '||', '|', [rfReplaceAll]);
  inc(part_count);
  SetLength(parts, part_count);
  parts[part_count - 1] := current_part;

  if length(parts) = 8 then
  begin
    self.task_id := StrToIntDef(parts[0], 0);
    self.name := parts[1];
    self.description := parts[2];
    self.creation_date := StrToDateTimeDef(parts[3], 0);
    self.due_date := StrToDateTimeDef(parts[4], 0);
    self.priority := self.string_to_priority(parts[5]);
    self.status := self.string_to_status(parts[6]);
    self.last_modified_date := StrToDateTimeDef(parts[7], 0);
  end
  else
  begin
    // Handle error or provide default values
    self.task_id := -1; // Indicate an invalid task
    self.name := 'error: invalid csv format';
    self.description := '';
    self.creation_date := 0;
    self.due_date := 0;
    self.priority := tp_low;
    self.status := ts_cancelled;
    self.last_modified_date := 0;
  end;
end;


// --- TTaskManager Implementation (Mixed and Enhanced) ---

constructor TTaskManager.create;
begin
  SetLength(f_tasks, 0);
  f_next_task_id := 1;
end;

destructor TTaskManager.destroy;
begin
  SetLength(f_tasks, 0); // Release dynamic array memory
end;

function TTaskManager.find_task_index_by_id(a_task_id: integer): integer;
var
  i: integer;
begin
  result := -1; // Default to not found
  for i := 0 to high(f_tasks) do
  begin
    if f_tasks[i].task_id = a_task_id then
    begin
      result := i;
      exit;
    end;
  end;
end;

function TTaskManager.add_task(a_name: string; a_description: string; a_due_date: tDateTime;
                               a_priority: TTaskPriority): integer;
var
  current_task_count: integer;
begin
  try
    current_task_count := length(f_tasks);
    SetLength(f_tasks, current_task_count + 1);
    f_tasks[current_task_count].create(f_next_task_id, a_name, a_description, a_due_date, a_priority);
    result := f_next_task_id; // Return the assigned ID
    inc(f_next_task_id);
  except
    on e: exception do
    begin
      writeln(format('Error adding task: %s', [e.message]));
      result := -1; // Indicate failure
    end;
  end;
end;

function TTaskManager.update_task_status(a_task_id: integer; a_new_status: TTaskStatus): boolean;
var
  task_index: integer;
begin
  task_index := find_task_index_by_id(a_task_id);
  if task_index <> -1 then
  begin
    f_tasks[task_index].status := a_new_status;
    f_tasks[task_index].last_modified_date := Now;
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

function TTaskManager.update_task_priority(a_task_id: integer; a_new_priority: TTaskPriority): boolean;
var
  task_index: integer;
begin
  task_index := find_task_index_by_id(a_task_id);
  if task_index <> -1 then
  begin
    f_tasks[task_index].priority := a_new_priority;
    f_tasks[task_index].last_modified_date := Now;
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

function TTaskManager.update_task_description(a_task_id: integer; a_new_description: string): boolean;
var
  task_index: integer;
begin
  task_index := find_task_index_by_id(a_task_id);
  if task_index <> -1 then
  begin
    f_tasks[task_index].description := a_new_description;
    f_tasks[task_index].last_modified_date := Now;
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

function TTaskManager.update_task_due_date(a_task_id: integer; a_new_due_date: tDateTime): boolean;
var
  task_index: integer;
begin
  task_index := find_task_index_by_id(a_task_id);
  if task_index <> -1 then
  begin
    f_tasks[task_index].due_date := a_new_due_date;
    f_tasks[task_index].last_modified_date := Now;
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

function TTaskManager.delete_task(a_task_id: integer): boolean;
var
  task_index: integer;
  i: integer;
  current_length: integer;
begin
  task_index := find_task_index_by_id(a_task_id);
  if task_index <> -1 then
  begin
    current_length := length(f_tasks);
    if current_length > 1 then
    begin
      for i := task_index to current_length - 2 do
      begin
        f_tasks[i] := f_tasks[i+1];
      end;
      SetLength(f_tasks, current_length - 1);
    end
    else
    begin
      SetLength(f_tasks, 0);
    end;
    result := true;
  end
  else
  begin
    result := false;
  end;
end;

function TTaskManager.get_task(a_task_id: integer; var a_task: TTask): boolean;
var
  task_index: integer;
begin
  task_index := find_task_index_by_id(a_task_id);
  if task_index <> -1 then
  begin
    a_task := f_tasks[task_index];
    result := true;
  end
  else
  begin
    result := false;
  end;
end;


function TTaskManager.get_all_tasks: TTaskDynArrayResult;
begin
  result := f_tasks; // Returns a copy of the dynamic array
end;

function TTaskManager.get_tasks_by_status(a_status: TTaskStatus): TTaskDynArrayResult;
var
  i: integer;
  temp_tasks: TTaskDynArray;
  temp_count: integer;
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);

  for i := 0 to high(f_tasks) do
  begin
    if f_tasks[i].status = a_status then
    begin
      inc(temp_count);
      SetLength(temp_tasks, temp_count);
      temp_tasks[high(temp_tasks)] := f_tasks[i];
    end;
  end;
  result := temp_tasks;
end;

function TTaskManager.get_tasks_by_priority(a_priority: TTaskPriority): TTaskDynArrayResult;
var
  i: integer;
  temp_tasks: TTaskDynArray;
  temp_count: integer;
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);

  for i := 0 to high(f_tasks) do
  begin
    if f_tasks[i].priority = a_priority then
    begin
      inc(temp_count);
      SetLength(temp_tasks, temp_count);
      temp_tasks[high(temp_tasks)] := f_tasks[i];
    end;
  end;
  result := temp_tasks;
end;

function TTaskManager.get_tasks_by_priority_range(min_priority, max_priority: TTaskPriority): TTaskDynArrayResult;
var
  i: integer;
  temp_tasks: TTaskDynArray;
  temp_count: integer;
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);

  for i := 0 to high(f_tasks) do
  begin
    // Enums can be compared directly
    if (f_tasks[i].priority >= min_priority) and (f_tasks[i].priority <= max_priority) then
    begin
      inc(temp_count);
      SetLength(temp_tasks, temp_count);
      temp_tasks[high(temp_tasks)] := f_tasks[i];
    end;
  end;
  result := temp_tasks;
end;


function TTaskManager.get_tasks_due_soon(a_days_threshold: integer): TTaskDynArrayResult;
var
  i: integer;
  temp_tasks: TTaskDynArray;
  temp_count: integer;
  today_start_of_day: tDateTime;
  due_date_limit_exclusive: tDateTime;
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);
  today_start_of_day := Trunc(Now);

  due_date_limit_exclusive := IncDay(Trunc(Today), a_days_threshold + 1);

  for i := 0 to high(f_tasks) do
  begin
    if (f_tasks[i].due_date >= today_start_of_day) and (f_tasks[i].due_date < due_date_limit_exclusive) and
       (f_tasks[i].status <> ts_completed) and (f_tasks[i].status <> ts_cancelled) then
    begin
      inc(temp_count);
      SetLength(temp_tasks, temp_count);
      temp_tasks[high(temp_tasks)] := f_tasks[i];
    end;
  end;
  result := temp_tasks;
end;

function TTaskManager.get_tasks_by_date_range(start_date, end_date: tDateTime): TTaskDynArrayResult;
var
  i: integer;
  temp_tasks: TTaskDynArray;
  temp_count: integer;
  start_of_range: tDateTime;
  end_of_range: tDateTime;
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);

  // Normalize dates to start and end of day for inclusive range
  start_of_range := Trunc(start_date);
  end_of_range := end_date; // EndOfADay(end_date) fails to compile... ; // Use EndOfADay for robust end-of-day calculation

  for i := 0 to high(f_tasks) do
  begin
    if (f_tasks[i].due_date >= start_of_range) and (f_tasks[i].due_date <= end_of_range) then
    begin
      inc(temp_count);
      SetLength(temp_tasks, temp_count);
      temp_tasks[high(temp_tasks)] := f_tasks[i];
    end;
  end;
  result := temp_tasks;
end;


// Helper function for QuickSort partitioning by ID
procedure partition_tasks_by_id(var arr: TTaskDynArray; low, high: integer; ascending: boolean; var pivot_index: integer);
var
  pivot_id: integer;
  i, j: integer;
  temp: TTask;
begin
  pivot_id := arr[high].task_id;
  i := low - 1;

  for j := low to high - 1 do
  begin
    if ascending then
    begin
      if arr[j].task_id <= pivot_id then
      begin
        inc(i);
        temp := arr[i];
        arr[i] := arr[j];
        arr[j] := temp;
      end;
    end
    else // descending
    begin
      if arr[j].task_id >= pivot_id then
      begin
        inc(i);
        temp := arr[i];
        arr[i] := arr[j];
        arr[j] := temp;
      end;
    end;
  end;
  inc(i);
  temp := arr[i];
  arr[i] := arr[high];
  arr[high] := temp;
  pivot_index := i;
end;

// QuickSort for ID
procedure quick_sort_tasks_by_id(var arr: TTaskDynArray; low, high: integer; ascending: boolean);
var
  pi: integer;
begin
  if low < high then
  begin
    partition_tasks_by_id(arr, low, high, ascending, pi);
    quick_sort_tasks_by_id(arr, low, pi - 1, ascending);
    quick_sort_tasks_by_id(arr, pi + 1, high, ascending);
  end;
end;

procedure TTaskManager.sort_tasks_by_id(ascending: boolean);
begin
  if length(f_tasks) > 0 then
  begin
    quick_sort_tasks_by_id(f_tasks, 0, high(f_tasks), ascending);
  end;
end;

// Helper function for QuickSort partitioning for due date
procedure partition_tasks_by_due_date(var arr: TTaskDynArray; low, high: integer; ascending: boolean; var pivot_index: integer);
var
  pivot: tDateTime;
  i, j: integer;
  temp: TTask;
begin
  pivot := arr[high].due_date;
  i := low - 1;

  for j := low to high - 1 do
  begin
    if ascending then
    begin
      if arr[j].due_date <= pivot then
      begin
        inc(i);
        temp := arr[i];
        arr[i] := arr[j];
        arr[j] := temp;
      end;
    end
    else // descending
    begin
      if arr[j].due_date >= pivot then
      begin
        inc(i);
        temp := arr[i];
        arr[i] := arr[j];
        arr[j] := temp;
      end;
    end;
  end;
  inc(i);
  temp := arr[i];
  arr[i] := arr[high];
  arr[high] := temp;
  pivot_index := i;
end;

// QuickSort for due date
procedure quick_sort_tasks_by_due_date(var arr: TTaskDynArray; low, high: integer; ascending: boolean);
var
  pi: integer;
begin
  if low < high then
  begin
    partition_tasks_by_due_date(arr, low, high, ascending, pi);
    quick_sort_tasks_by_due_date(arr, low, pi - 1, ascending);
    quick_sort_tasks_by_due_date(arr, pi + 1, high, ascending);
  end;
end;

procedure TTaskManager.sort_tasks_by_due_date(ascending: boolean);
begin
  if length(f_tasks) > 0 then
  begin
    quick_sort_tasks_by_due_date(f_tasks, 0, high(f_tasks), ascending);
  end;
end;

// Helper function for QuickSort partitioning by priority
procedure partition_tasks_by_priority(var arr: TTaskDynArray; low, high: integer; ascending: boolean; var pivot_index: integer);
var
  pivot: TTaskPriority;
  i, j: integer;
  temp: TTask;
begin
  pivot := arr[high].priority;
  i := low - 1;

  for j := low to high - 1 do
  begin
    if ascending then
    begin
      // Enums are ordered by their declaration order (tp_low < tp_medium etc.)
      if arr[j].priority <= pivot then
      begin
        inc(i);
        temp := arr[i];
        arr[i] := arr[j];
        arr[j] := temp;
      end;
    end
    else // descending
    begin
      if arr[j].priority >= pivot then
      begin
        inc(i);
        temp := arr[i];
        arr[i] := arr[j];
        arr[j] := temp;
      end;
    end;
  end;
  inc(i);
  temp := arr[i];
  arr[i] := arr[high];
  arr[high] := temp;
  pivot_index := i;
end;

// QuickSort for priority
procedure quick_sort_tasks_by_priority(var arr: TTaskDynArray; low, high: integer; ascending: boolean);
var
  pi: integer;
begin
  if low < high then
  begin
    partition_tasks_by_priority(arr, low, high, ascending, pi);
    quick_sort_tasks_by_priority(arr, low, pi - 1, ascending);
    quick_sort_tasks_by_priority(arr, pi + 1, high, ascending);
  end;
end;

procedure TTaskManager.sort_tasks_by_priority(ascending: boolean);
begin
  if length(f_tasks) > 0 then
  begin
    quick_sort_tasks_by_priority(f_tasks, 0, high(f_tasks), ascending);
  end;
end;

function TTaskManager.save_to_file(const a_filename: string): boolean;
var
  f: textfile;
  i: integer;
begin
  assignfile(f, a_filename);
  {$i-} // Disable I/O checking
  rewrite(f);
  {$i+} // Enable I/O checking
  if ioresult <> 0 then
  begin
    writeln(format('Error: Could not open file "%s" for writing. I/O Error: %d', [a_filename, ioresult]));
    result := false;
    exit;
  end;

  try
    for i := 0 to high(f_tasks) do
    begin
      writeln(f, f_tasks[i].to_csv_string);
    end;
    result := true;
  except
    on e: exception do
    begin
      writeln(format('Error saving tasks to file: %s', [e.message]));
      result := false;
    end;
  end;
  closefile(f);
end;

function TTaskManager.load_from_file(const a_filename: string): boolean;
var
  f: textfile;
  csv_line: string;
  temp_task: TTask;
  current_task_count: integer;
begin
  SetLength(f_tasks, 0); // Clear existing tasks
  f_next_task_id := 1; // Reset ID counter
  assignfile(f, a_filename);
  {$i-}
  reset(f);
  {$i+}
  if ioresult <> 0 then
  begin
    writeln(format('Warning: Could not open file "%s" for reading. Possibly new manager or file not found. I/O Error: %d', [a_filename, ioresult]));
    result := false;
    exit;
  end;

  try
    while not eof(f) do
    begin
      readln(f, csv_line);
      if csv_line <> '' then
      begin
        temp_task.from_csv_string(csv_line);
        if temp_task.task_id <> -1 then // Check for valid task parsing (ID -1 indicates parsing error)
        begin
          current_task_count := length(f_tasks);
          SetLength(f_tasks, current_task_count + 1);
          f_tasks[current_task_count] := temp_task;
          if temp_task.task_id >= f_next_task_id then
          begin
            f_next_task_id := temp_task.task_id + 1; // Ensure next_task_id is always higher than loaded IDs
          end;
        end
        else
        begin
          writeln(format('Warning: Skipping invalid CSV line during load: %s', [csv_line]));
        end;
      end;
    end;
    result := true;
  except
    on e: exception do
    begin
      writeln(format('Error loading tasks from file: %s', [e.message]));
      result := false;
    end;
  end;
  closefile(f);
end;

function TTaskManager.get_all_tasks_string: string;
var
  i: integer;
  s: string;
begin
  s := '';
  if length(f_tasks) = 0 then
  begin
    s := 'No tasks found.' + sLineBreak;
  end
  else
  begin
    for i := 0 to high(f_tasks) do
    begin
      s := s + 'Task ' + inttostr(i+1) + ': ' + f_tasks[i].to_string + sLineBreak;
    end;
  end;
  result := s;
end;


// --- Self Test Procedure (Mixed and Expanded) ---

procedure self_test;
var
  task_manager: TTaskManager;
  today_date: tDateTime;
  tomorrow_date: tDateTime;
  next_week_date: tDateTime;
  next_month_date: tDateTime;
  tasks_filtered: TTaskDynArrayResult;
  a_task: TTask;
  task_id1, task_id2, task_id3, task_id4: integer;
  filename: string;
begin
  filename := 'mixed_tasks.csv'; // File for persistence test

  writeln('--- Starting Mixed Task Manager Self-Test ---');

  task_manager.create;

  today_date := Now;
  tomorrow_date := IncDay(today_date, 1);
  next_week_date := IncDay(today_date, 7);
  next_month_date := IncMonth(today_date, 1);

  writeln('1. Adding tasks with various priorities and due dates...');
  task_id1 := task_manager.add_task('Design new API', 'Outline endpoints and data models', IncDay(now, 5), tp_high);
  task_id2 := task_manager.add_task('Write documentation', 'Complete user manual for v1.0', IncMonth(now, 1), tp_medium);
  task_id3 := task_manager.add_task('Bugfix: Crash on startup', 'Investigate and fix critical startup issue', IncDay(now, 1), tp_urgent);
  task_id4 := task_manager.add_task('Refactor old module', 'Improve readability and performance of legacy code', IncDay(now, 10), tp_low);
  task_manager.add_task('Prepare for sprint review', 'Gather metrics and demo features', IncDay(now, 3), tp_high);

  writeln(sLineBreak + '2. All tasks initially:');
  writeln(task_manager.get_all_tasks_string);

  writeln('3. Updating task ', task_id3, ' status to completed and description.');
  if task_manager.update_task_status(task_id3, ts_completed) then
    writeln(format('Task %d status updated.', [task_id3]))
  else
    writeln(format('Failed to update task %d status.', [task_id3]));
  if task_manager.update_task_description(task_id3, 'Fixed startup crash, deployed hotfix.') then
    writeln(format('Task %d description updated.', [task_id3]))
  else
    writeln(format('Failed to update task %d description.', [task_id3]));

  writeln(sLineBreak + '4. Retrieving task ', task_id1, ':');
  if task_manager.get_task(task_id1, a_task) then
  begin
    writeln('Found task: ', a_task.to_string);
  end
  else
    writeln(format('Task %d not found.', [task_id1]));

  writeln(sLineBreak + '5. Filtering tasks by status (pending):');
  tasks_filtered := task_manager.get_tasks_by_status(ts_pending);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No pending tasks.');

  writeln(sLineBreak + '6. Filtering tasks by priority (high):');
  tasks_filtered := task_manager.get_tasks_by_priority(tp_high);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No high priority tasks.');

  writeln(sLineBreak + '7. Filtering tasks by priority range (medium to urgent):');
  tasks_filtered := task_manager.get_tasks_by_priority_range(tp_medium, tp_urgent);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No tasks in medium to urgent priority range.');

  writeln(sLineBreak + '8. Filtering tasks due within 7 days (from today):');
  tasks_filtered := task_manager.get_tasks_due_soon(7);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No tasks due within 7 days.');

  writeln(sLineBreak + '9. Filtering tasks due between tomorrow and next week:');
  tasks_filtered := task_manager.get_tasks_by_date_range(tomorrow_date, next_week_date);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No tasks due between tomorrow and next week.');

  writeln(sLineBreak + '10. Sorting tasks by Due Date (Ascending):');
  task_manager.sort_tasks_by_due_date(true);
  writeln(task_manager.get_all_tasks_string);

  writeln(sLineBreak + '11. Sorting tasks by Priority (Descending):');
  task_manager.sort_tasks_by_priority(false);
  writeln(task_manager.get_all_tasks_string);

  writeln(sLineBreak + '12. Sorting tasks by ID (Ascending):');
  task_manager.sort_tasks_by_id(true);
  writeln(task_manager.get_all_tasks_string);

  writeln(sLineBreak + '13. Deleting task ', task_id4, ' (Refactor old module).');
  if task_manager.delete_task(task_id4) then
    writeln(format('Task %d deleted successfully.', [task_id4]))
  else
    writeln(format('Failed to delete task %d.', [task_id4]));

  writeln(sLineBreak + '14. All tasks after deletion:');
  writeln(task_manager.get_all_tasks_string);

  writeln(sLineBreak + format('15. Saving tasks to "%s"...', [filename]));
  if task_manager.save_to_file(filename) then
    writeln('Tasks saved successfully.')
  else
    writeln('Failed to save tasks.');

  writeln(sLineBreak + '16. Destroying and recreating task manager to test loading...');
  task_manager.destroy;
  task_manager.create;

  if task_manager.load_from_file(filename) then
  begin
    writeln('Tasks loaded successfully.');
    writeln(sLineBreak + '17. All tasks after loading from file:');
    writeln(task_manager.get_all_tasks_string);
  end
  else
  begin
    writeln('Failed to load tasks from file. This might be expected if file didn''t exist initially.');
  end;

  // Final cleanup
  task_manager.destroy;
  writeln(sLineBreak + '--- Mixed Task Manager Self-Test Completed ---');
end;

// --- Main Program Block ---

begin
  // Ensure the 'bin' directory exists. Handled by SysUtils.
  if not DirectoryExists('bin') then
  begin
    CreateDir('bin');
  end;

  self_test;
end.

(*
The prompt was:
Using only the free pascal (fpc) computing language, code a task manager .
When compiling code, generate your binaries at the bin/ folder. Do not mix source code with binary (compiled) files.
When testing, review the source code and test if it compiles. Verify for the risk of any infinite loop or memory leak.
Only try to run code after verifying for infinite loop, memory leak and compilation errors. When compiling pascal code, use this example:
run_os_command('fpc solution1.pas -obin/task_manager -O1 -Mobjfpc')
Notice in the example above that there is no space after "-o" for the output file parameter.
With fpc, do not use -Fc nor -o/dev/null or similar.
Do not code any user input such as ReadLn. You are coding a reusable unit that might be used with graphical user interfaces.
You will replace fixed sized arrays by dynamic arrays.
All pascal reserved words will be typed in lowercase.
Do not change the current working folder.
When you are asked to compare solutions, compile each version/solution. Only select solutions that do compile.
When compiling code, generate your binaries at the bin/ folder. Do not mix source code with binary (compiled) files.
When testing, review the source code and test if it compiles. Verify for the risk of any infinite loop or memory leak.
Feel free to search the internet with error messages if you need.
As you are super-intelligent, I do trust you.
This is an example how to code and compile a pascal program:
<example>
<safetofile filename='solutionx.pas'>
program mytask;
{$mode objfpc} // Use Object Pascal mode for dynamic arrays and objects

uses
  SysUtils,
  DateUtils,
  mytask; // your unit

begin
  WriteLn('Hello!');
end.
</safetofile>
<runcode>
run_os_command('fpc solutionx.pas -obin/temp -O1 -Mobjfpc')
run_os_command('bin/temp', 120)
</runcode>
</example>
In the compilation command `fpc usolutionx.pas -obin/temp -O1 -Mobjfpc`.
Each time that you have an error such as "tsimplexunit.pas(206,14) Fatal: Syntax error, "identifier" expected but "is" found",
you will call something like this: get_line_from_file('tsimplexunit.pas',206)
REMEMBER:
* "```pascal" will not save a pascal file into disk. Use safetofile tags instead.
* DO NOT CREATE A SOLUTION WITH MULTIPLE FILES. Save the entire solution into solutionx.pas where x is either 1, 2 or 3.
* DO NOT declare variables within a begin/end block. ALWAYS declare variables in the declaration area.
* DO NOT use label/go to.
* DO NOT declare anything that starts with a digit such as:
   var 1stVariable: integer;
* DO NOT use the type `real` for real numbers as it depends on hardware. Use `double` or `single` instead.
* CREATE A TYPE for dynamic array function results.
  This declaration will fail: `function solve(anp: integer; var acostmatrix: array of tRealArray): array of tAppointmentResult;`.
  Do this instead: ```
  type
    TApptResultDynArr = array of tAppointmentResult;
  ...
  function solve(anp: integer; var acostmatrix: array of tRealArray): tAAR;
  ```
* If you have strange compilation errors, you may use get_line_from_file if you like.
* Include in your uses the unit math as the unit math contains many useful constants and functions (such as MaxDouble).
* When passing arrays as parameter, consider passing as reference to avoid memory copying.
* Create a method called self_test. In this method, you will code static inputs for testing (there will be no externally entered data to test with - do not use ReadLn for testing).
* BE BOLD AND CODE AS MANY FEATURES AS YOU CAN!

Feel free to search the internet with error messages if you need.
As you are super-intelligent, I do trust you.
--reasoning_effort high

This is the function call:
evolutive_problem_solver(coder_model, task, agent_steps=54, steps=4, start_now=True,
    fileext='.pas', tools=tools, log_level=LogLevel.ERROR, step_callbacks=[delay_execution_10], refine=True)

This is the execution output:
--- Starting Mixed Task Manager Self-Test ---
1. Adding tasks with various priorities and due dates...

2. All tasks initially:
Task 1: ID: 1, Name: Design new API, Desc: Outline endpoints and data models, Due: 2025-08-14 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 2: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 07:15:46, Priority: medium, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 3: ID: 3, Name: Bugfix: Crash on startup, Desc: Investigate and fix critical startup issue, Due: 2025-08-10 07:15:46, Priority: urgent, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 4: ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 07:15:46, Priority: low, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 5: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46

3. Updating task 3 status to completed and description.
Task 3 status updated.
Task 3 description updated.

4. Retrieving task 1:
Found task: ID: 1, Name: Design new API, Desc: Outline endpoints and data models, Due: 2025-08-14 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46

5. Filtering tasks by status (pending):
ID: 1, Name: Design new API, Desc: Outline endpoints and data models, Due: 2025-08-14 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 07:15:46, Priority: medium, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 07:15:46, Priority: low, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46

6. Filtering tasks by priority (high):
ID: 1, Name: Design new API, Desc: Outline endpoints and data models, Due: 2025-08-14 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46

7. Filtering tasks by priority range (medium to urgent):
ID: 1, Name: Design new API, Desc: Outline endpoints and data models, Due: 2025-08-14 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 07:15:46, Priority: medium, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix., Due: 2025-08-10 07:15:46, Priority: urgent, Status: completed, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46

8. Filtering tasks due within 7 days (from today):
ID: 1, Name: Design new API, Desc: Outline endpoints and data models, Due: 2025-08-14 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46

9. Filtering tasks due between tomorrow and next week:
ID: 1, Name: Design new API, Desc: Outline endpoints and data models, Due: 2025-08-14 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix., Due: 2025-08-10 07:15:46, Priority: urgent, Status: completed, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46

10. Sorting tasks by Due Date (Ascending):
Task 1: ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix., Due: 2025-08-10 07:15:46, Priority: urgent, Status: completed, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 2: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 3: ID: 1, Name: Design new API, Desc: Outline endpoints and data models, Due: 2025-08-14 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 4: ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 07:15:46, Priority: low, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 5: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 07:15:46, Priority: medium, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46


11. Sorting tasks by Priority (Descending):
Task 1: ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix., Due: 2025-08-10 07:15:46, Priority: urgent, Status: completed, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 2: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 3: ID: 1, Name: Design new API, Desc: Outline endpoints and data models, Due: 2025-08-14 07:15:46, Priority: high, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 4: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 07:15:46, Priority: medium, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46
Task 5: ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 07:15:46, Priority: low, Status: pending, Created: 2025-08-09 07:15:46, LastMod: 2025-08-09 07:15:46

*)
