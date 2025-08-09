// This source code was created by "evolutive_problem_solver" from https://github.com/joaopauloschuler/beyond-python-smolagents.
// You can find the prompt and the used parameters at the end of this file.

program TaskManagerApp;

{$mode objfpc} // Use Object Pascal mode for dynamic arrays and objects
{$h+}          // Enable AnsiStrings by default

uses
  SysUtils,    // For TDateTime, DateTimeToStr, StrToDateTime, Exception, sLineBreak, StringReplace, StrToIntDef, StrToDateTimeDef, Now, Today, Trunc, DirectoryExists, CreateDir, UpperCase, DecodeDate, FormatDateTime
  DateUtils,   // For DaysBetween, IncDay, IncMonth, IncYear, EncodeDate, EndOfADay // EndOfADay is here
  Math;        // For mathematical functions and constants (like MaxDouble)

// --- Constants ---
const
  TASK_DATA_FILENAME = 'mixed_tasks.csv';
  MALFORMED_TASK_DATA_FILENAME = 'malformed_tasks.csv'; // For testing error handling
  DATE_TIME_FORMAT = 'yyyy-mm-dd hh:nn:ss'; // Standard format for display and CSV

// --- Type Definitions (Extensive and Rich) ---

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
    ts_cancelled,
    ts_archived // NEW: Added status for archiving tasks
  );

  TTask = object // Encapsulates task data and behavior
    task_id: integer;
    name: string;
    description: string;
    creation_date: tDateTime;
    due_date: tDateTime;
    priority: TTaskPriority;
    status: TTaskStatus;
    last_modified_date: tDateTime; // Tracks last modification for any field

    constructor create(a_task_id: integer; a_name: string; a_description: string;
                       a_due_date: tDateTime; a_priority: TTaskPriority);

    // --- Enum Conversion Helpers ---
    function priority_to_string: string;
    function status_to_string: string;
    function string_to_priority(const s: string): TTaskPriority;
    function string_to_status(const s: string): TTaskStatus;

    // --- Setter Methods (update last_modified_date automatically) ---
    procedure set_status(a_new_status: TTaskStatus);
    procedure set_priority(a_new_priority: TTaskPriority);
    procedure set_description(a_new_description: string);
    procedure set_due_date(a_new_due_date: tDateTime);
    procedure set_name(a_new_name: string);

    // --- Data Representation ---
    function to_string: string; // Formatted string for display
    function to_csv_string: string; // CSV formatted string for persistence
    procedure from_csv_string(const csv_line: string); // Populates from CSV string
  end;

  // Explicit dynamic array types (as required)
  TStringDynArray = array of string;
  TTaskDynArray = array of TTask;
  TTaskDynArrayResult = array of TTask; // For function results

  TTaskManager = object
  private
    f_tasks: TTaskDynArray;
    f_next_task_id: integer; // Ensures unique task IDs

    // --- Private Helper Methods ---
    function find_task_index_by_id(a_task_id: integer): integer; // Finds index for internal ops
    function filter_active_tasks(const source_tasks: TTaskDynArray): TTaskDynArrayResult; // Filters out archived tasks
    function filter_non_excluded_statuses(const source_tasks: TTaskDynArray): TTaskDynArrayResult; // Filters out completed, cancelled, archived tasks

  public
    constructor create;
    destructor destroy;

    // --- CRUD Operations ---
    function add_task(a_name: string; a_description: string; a_due_date: tDateTime;
                      a_priority: TTaskPriority): integer; // Returns assigned Task ID
    function update_task_status(a_task_id: integer; a_new_status: TTaskStatus): boolean;
    function update_task_priority(a_task_id: integer; a_new_priority: TTaskPriority): boolean;
    function update_task_description(a_task_id: integer; a_new_description: string): boolean;
    function update_task_due_date(a_task_id: integer; a_new_due_date: tDateTime): boolean;
    function update_task_name(a_task_id: integer; a_new_name: string): boolean;
    function delete_task(a_task_id: integer): boolean; // Permanently deletes task
    function archive_task(a_task_id: integer): boolean; // Archives a task (changes status)
    function restore_task(a_task_id: integer): boolean; // Restores an archived task (sets status to pending)
    function get_task(a_task_id: integer; var a_task: TTask): boolean; // Retrieves a task object by ID

    // --- Query and Retrieval (defaulting to active tasks) ---
    function get_all_tasks: TTaskDynArrayResult; // Retrieves all *active* tasks
    function get_tasks_by_status(a_status: TTaskStatus): TTaskDynArrayResult; // Retrieves tasks by specified status (includes archived if status is ts_archived)
    function get_tasks_by_priority(a_priority: TTaskPriority): TTaskDynArrayResult; // Retrieves *active* tasks by priority
    function get_tasks_by_priority_range(min_priority, max_priority: TTaskPriority): TTaskDynArrayResult; // Retrieves *active* tasks within priority range
    function get_tasks_due_soon(a_days_threshold: integer): TTaskDynArrayResult; // Retrieves *active*, non-completed/cancelled tasks due soon
    function get_tasks_by_date_range(start_date, end_date: tDateTime): TTaskDynArrayResult; // Retrieves *active* tasks within date range
    function get_overdue_tasks: TTaskDynArrayResult; // Retrieves *active*, non-completed/cancelled tasks that are overdue
    function get_tasks_by_name_contains(a_keyword: string): TTaskDynArrayResult; // Case-insensitive search in task name (active tasks only)
    function get_tasks_by_description_contains(a_keyword: string): TTaskDynArrayResult; // Case-insensitive search in task description (active tasks only)
    function get_archived_tasks: TTaskDynArrayResult; // NEW: Retrieves only archived tasks
    function get_all_tasks_including_archived: TTaskDynArrayResult; // NEW: Retrieves all tasks regardless of status

    // --- Sorting ---
    procedure sort_tasks_by_id(ascending: boolean);
    procedure sort_tasks_by_due_date(ascending: boolean);
    procedure sort_tasks_by_priority(ascending: boolean);

    // --- Persistence ---
    function save_to_file(const a_filename: string): boolean;
    function load_from_file(const a_filename: string): boolean;

    // --- Display Utility ---
    function get_all_tasks_string: string; // Returns string representation of *active* tasks
    function get_all_tasks_string_including_archived: string; // Returns string representation of *all* tasks
  end;

// --- TTask Implementation ---

constructor TTask.create(a_task_id: integer; a_name: string; a_description: string;
                       a_due_date: tDateTime; a_priority: TTaskPriority);
begin
  self.task_id := a_task_id;
  self.name := a_name;
  self.description := a_description;
  self.creation_date := Now;
  self.due_date := a_due_date;
  self.priority := a_priority;
  self.status := ts_pending; // Default status for new tasks
  self.last_modified_date := Now;
end;

function TTask.priority_to_string: string;
begin
  case self.priority of
    tp_low: result := 'low';
    tp_medium: result := 'medium';
    tp_high: result := 'high';
    tp_urgent: result := 'urgent';
  else
    result := 'unknown'; // Safeguard for unexpected enum values
  end;
end;

function TTask.status_to_string: string;
begin
  case self.status of
    ts_pending: result := 'pending';
    ts_in_progress: result := 'in_progress';
    ts_completed: result := 'completed';
    ts_cancelled: result := 'cancelled';
    ts_archived: result := 'archived'; // NEW
  else
    result := 'unknown'; // Safeguard
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
  else if s = 'archived' then result := ts_archived // NEW
  else result := ts_pending; // Default or error handling
end;

procedure TTask.set_status(a_new_status: TTaskStatus);
begin
  self.status := a_new_status;
  self.last_modified_date := Now;
end;

procedure TTask.set_priority(a_new_priority: TTaskPriority);
begin
  self.priority := a_new_priority;
  self.last_modified_date := Now;
end;

procedure TTask.set_description(a_new_description: string);
begin
  self.description := a_new_description;
  self.last_modified_date := Now;
end;

procedure TTask.set_due_date(a_new_due_date: tDateTime);
begin
  self.due_date := a_new_due_date;
  self.last_modified_date := Now;
end;

procedure TTask.set_name(a_new_name: string);
begin
  self.name := a_new_name;
  self.last_modified_date := Now;
end;

function TTask.to_string: string;
begin
  result := format('ID: %d, Name: %s, Desc: %s, Due: %s, Priority: %s, Status: %s, Created: %s, LastMod: %s',
                   [self.task_id,
                    self.name,
                    self.description,
                    FormatDateTime(DATE_TIME_FORMAT, self.due_date),
                    self.priority_to_string,
                    self.status_to_string,
                    FormatDateTime(DATE_TIME_FORMAT, self.creation_date),
                    FormatDateTime(DATE_TIME_FORMAT, self.last_modified_date)]);
end;

function TTask.to_csv_string: string;
begin
  // Using pipes as delimiters; doubling pipes '||' to escape them within fields.
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
    // Indicate an invalid task if CSV format is incorrect
    self.task_id := -1; // Sentinel value for invalid
    self.name := ''; // Clear fields for invalid tasks
    self.description := '';
    self.creation_date := 0;
    self.due_date := 0;
    self.priority := tp_low;
    self.status := ts_cancelled;
    self.last_modified_date := 0;
  end;
end;


// --- TTaskManager Implementation ---

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

function TTaskManager.filter_active_tasks(const source_tasks: TTaskDynArray): TTaskDynArrayResult;
var
  i: integer;
  temp_tasks: TTaskDynArray;
  temp_count: integer;
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);

  for i := 0 to high(source_tasks) do
  begin
    if source_tasks[i].status <> ts_archived then
    begin
      inc(temp_count);
      SetLength(temp_tasks, temp_count);
      temp_tasks[high(temp_tasks)] := source_tasks[i];
    end;
  end;
  result := temp_tasks;
end;

function TTaskManager.filter_non_excluded_statuses(const source_tasks: TTaskDynArray): TTaskDynArrayResult;
var
  i: integer;
  temp_tasks: TTaskDynArray;
  temp_count: integer;
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);

  for i := 0 to high(source_tasks) do
  begin
    if (source_tasks[i].status <> ts_completed) and
       (source_tasks[i].status <> ts_cancelled) and
       (source_tasks[i].status <> ts_archived) then
    begin
      inc(temp_count);
      SetLength(temp_tasks, temp_count);
      temp_tasks[high(temp_tasks)] := source_tasks[i];
    end;
  end;
  result := temp_tasks;
end;

function TTaskManager.add_task(a_name: string; a_description: string; a_due_date: tDateTime;
                               a_priority: TTaskPriority): integer;
var
  current_task_count: integer;
begin
  // Input Validation
  if trim(a_name) = '' then
  begin
    writeln('Validation Error: Task name cannot be empty.');
    result := -1; // Indicate failure
    exit;
  end;

  if a_due_date = 0 then // Check for zero date
  begin
    writeln('Validation Error: Due date cannot be a zero date.');
    result := -1;
    exit;
  end;

  // Optionally, disallow due dates in the past for new tasks (can be adjusted based on requirements)
  if Trunc(a_due_date) < Trunc(Now) then
  begin
     writeln('Validation Error: Due date cannot be in the past for a new task.');
     result := -1;
     exit;
  end;

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
    f_tasks[task_index].set_status(a_new_status);
    result := true;
  end
  else
  begin
    writeln(format('Update Error: Task with ID %d not found for status update.', [a_task_id]));
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
    f_tasks[task_index].set_priority(a_new_priority);
    result := true;
  end
  else
  begin
    writeln(format('Update Error: Task with ID %d not found for priority update.', [a_task_id]));
    result := false;
  end;
end;

function TTaskManager.update_task_description(a_task_id: integer; a_new_description: string): boolean;
var
  task_index: integer;
begin
  // Input Validation
  if trim(a_new_description) = '' then
  begin
    writeln('Validation Error: Task description cannot be empty for update.');
    result := false;
    exit;
  end;

  task_index := find_task_index_by_id(a_task_id);
  if task_index <> -1 then
  begin
    f_tasks[task_index].set_description(a_new_description);
    result := true;
  end
  else
  begin
    writeln(format('Update Error: Task with ID %d not found for description update.', [a_task_id]));
    result := false;
  end;
end;

function TTaskManager.update_task_due_date(a_task_id: integer; a_new_due_date: tDateTime): boolean;
var
  task_index: integer;
begin
  // Input Validation
  if a_new_due_date = 0 then // Check for zero date
  begin
    writeln('Validation Error: New due date cannot be a zero date for update.');
    result := false;
    exit;
  end;

  task_index := find_task_index_by_id(a_task_id);
  if task_index <> -1 then
  begin
    f_tasks[task_index].set_due_date(a_new_due_date);
    result := true;
  end
  else
  begin
    writeln(format('Update Error: Task with ID %d not found for due date update.', [a_task_id]));
    result := false;
  end;
end;

function TTaskManager.update_task_name(a_task_id: integer; a_new_name: string): boolean;
var
  task_index: integer;
begin
  // Input Validation
  if trim(a_new_name) = '' then
  begin
    writeln('Validation Error: Task name cannot be empty for update.');
    result := false;
    exit;
  end;

  task_index := find_task_index_by_id(a_task_id);
  if task_index <> -1 then
  begin
    f_tasks[task_index].set_name(a_new_name);
    result := true;
  end
  else
  begin
    writeln(format('Update Error: Task with ID %d not found for name update.', [a_task_id]));
    result := false;
  end;
end;

function TTaskManager.archive_task(a_task_id: integer): boolean;
var
  task_index: integer;
begin
  task_index := find_task_index_by_id(a_task_id);
  if task_index <> -1 then
  begin
    f_tasks[task_index].set_status(ts_archived);
    result := true;
  end
  else
  begin
    writeln(format('Action Error: Task with ID %d not found for archiving.', [a_task_id]));
    result := false;
  end;
end;

function TTaskManager.restore_task(a_task_id: integer): boolean;
var
  task_index: integer;
begin
  task_index := find_task_index_by_id(a_task_id);
  if task_index <> -1 then
  begin
    // Restore to pending status
    if f_tasks[task_index].status = ts_archived then
    begin
      f_tasks[task_index].set_status(ts_pending);
      result := true;
    end
    else
    begin
      writeln(format('Action Error: Task with ID %d is not archived, cannot restore.', [a_task_id]));
      result := false;
    end;
  end
  else
  begin
    writeln(format('Action Error: Task with ID %d not found for restoration.', [a_task_id]));
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
      SetLength(f_tasks, 0); // Clear array if last item deleted
    end;
    result := true;
  end
  else
  begin
    writeln(format('Action Error: Task with ID %d not found for deletion.', [a_task_id]));
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
    a_task := f_tasks[task_index]; // Returns a copy of the task object
    result := true;
  end
  else
  begin
    // No writeln here, as not finding a task is often a valid query result, not an error.
    result := false;
  end;
end;

function TTaskManager.get_all_tasks: TTaskDynArrayResult;
begin
  // Retrieves all *active* (non-archived) tasks by default
  result := filter_active_tasks(f_tasks);
end;

function TTaskManager.get_all_tasks_including_archived: TTaskDynArrayResult;
begin
  // Retrieves all tasks, regardless of status
  result := f_tasks;
end;

function TTaskManager.get_archived_tasks: TTaskDynArrayResult;
var
  i: integer;
  temp_tasks: TTaskDynArray;
  temp_count: integer;
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);

  for i := 0 to high(f_tasks) do
  begin
    if f_tasks[i].status = ts_archived then
    begin
      inc(temp_count);
      SetLength(temp_tasks, temp_count);
      temp_tasks[high(temp_tasks)] := f_tasks[i];
    end;
  end;
  result := temp_tasks;
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
    // This function returns tasks of the *specified* status, including archived if ts_archived is requested.
    // No implicit filtering for active tasks.
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
    if (f_tasks[i].priority = a_priority) and (f_tasks[i].status <> ts_archived) then // Exclude archived by default
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
    if (f_tasks[i].priority >= min_priority) and (f_tasks[i].priority <= max_priority) and
       (f_tasks[i].status <> ts_archived) then // Exclude archived by default
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
    // Only return tasks that are due soon and are not completed, cancelled, or archived
    if (f_tasks[i].due_date >= today_start_of_day) and (f_tasks[i].due_date < due_date_limit_exclusive) and
       (f_tasks[i].status <> ts_completed) and (f_tasks[i].status <> ts_cancelled) and
       (f_tasks[i].status <> ts_archived) then
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
  y, m, d: word; // For DecodeDate
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);

  // Normalize dates to start and end of day for inclusive range
  start_of_range := Trunc(start_date);
  decodedate(end_date, y, m, d);
  end_of_range := EndOfADay(y, m, d);

  for i := 0 to high(f_tasks) do
  begin
    if (f_tasks[i].due_date >= start_of_range) and (f_tasks[i].due_date <= end_of_range) and
       (f_tasks[i].status <> ts_archived) then // Exclude archived by default
    begin
      inc(temp_count);
      SetLength(temp_tasks, temp_count);
      temp_tasks[high(temp_tasks)] := f_tasks[i];
    end;
  end;
  result := temp_tasks;
end;

function TTaskManager.get_overdue_tasks: TTaskDynArrayResult;
var
  i: integer;
  temp_tasks: TTaskDynArray;
  temp_count: integer;
  today_start_of_day: tDateTime;
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);
  today_start_of_day := Trunc(Now);

  for i := 0 to high(f_tasks) do
  begin
    // A task is overdue if its due date is before today AND it's not completed, cancelled or archived
    if (Trunc(f_tasks[i].due_date) < today_start_of_day) and
       (f_tasks[i].status <> ts_completed) and
       (f_tasks[i].status <> ts_cancelled) and
       (f_tasks[i].status <> ts_archived) then
    begin
      inc(temp_count);
      SetLength(temp_tasks, temp_count);
      temp_tasks[high(temp_tasks)] := f_tasks[i];
    end;
  end;
  result := temp_tasks;
end;

function TTaskManager.get_tasks_by_name_contains(a_keyword: string): TTaskDynArrayResult;
var
  i: integer;
  temp_tasks: TTaskDynArray;
  temp_count: integer;
  upper_keyword: string;
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);
  upper_keyword := uppercase(a_keyword); // Case-insensitive search

  for i := 0 to high(f_tasks) do
  begin
    if (pos(upper_keyword, uppercase(f_tasks[i].name)) > 0) and (f_tasks[i].status <> ts_archived) then // Exclude archived by default
    begin
      inc(temp_count);
      SetLength(temp_tasks, temp_count);
      temp_tasks[high(temp_tasks)] := f_tasks[i];
    end;
  end;
  result := temp_tasks;
end;

function TTaskManager.get_tasks_by_description_contains(a_keyword: string): TTaskDynArrayResult;
var
  i: integer;
  temp_tasks: TTaskDynArray;
  temp_count: integer;
  upper_keyword: string;
begin
  temp_count := 0;
  SetLength(temp_tasks, 0);
  upper_keyword := uppercase(a_keyword); // Case-insensitive search

  for i := 0 to high(f_tasks) do
  begin
    if (pos(upper_keyword, uppercase(f_tasks[i].description)) > 0) and (f_tasks[i].status <> ts_archived) then // Exclude archived by default
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
  // Sorting impacts the internal f_tasks array, which contains all tasks (active and archived)
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
      if trim(csv_line) <> '' then // Also check for empty lines
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
          writeln(format('Loading Warning: Skipping malformed CSV line: "%s" (Error parsing task data)', [csv_line]));
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
  active_tasks: TTaskDynArray;
begin
  s := '';
  active_tasks := self.get_all_tasks; // Get only active tasks
  if length(active_tasks) = 0 then
  begin
    s := 'No active tasks found.' + sLineBreak;
  end
  else
  begin
    for i := 0 to high(active_tasks) do
    begin
      s := s + 'Task ' + inttostr(active_tasks[i].task_id) + ': ' + active_tasks[i].to_string + sLineBreak;
    end;
  end;
  result := s;
end;

function TTaskManager.get_all_tasks_string_including_archived: string;
var
  i: integer;
  s: string;
begin
  s := '';
  if length(f_tasks) = 0 then
  begin
    s := 'No tasks found (including archived).' + sLineBreak;
  end
  else
  begin
    for i := 0 to high(f_tasks) do
    begin
      s := s + 'Task ' + inttostr(f_tasks[i].task_id) + ': ' + f_tasks[i].to_string + sLineBreak;
    end;
  end;
  result := s;
end;


// --- Self Test Procedure (Extensive and Detailed) ---

procedure self_test;
var
  task_manager: TTaskManager;
  today_date: tDateTime;
  tomorrow_date: tDateTime;
  last_week_date: tDateTime;
  tasks_filtered: TTaskDynArrayResult;
  a_task: TTask;
  task_id1, task_id2, task_id3, task_id4, task_id_archive_test, task_id_restore_test: integer;
  non_existent_id: integer; // For testing operations on non-existent IDs
  success: boolean; // For tracking boolean results from functions
begin
  writeln('--- Starting Comprehensive Task Manager Self-Test (with Enhanced Error Handling) ---');

  task_manager.create;

  today_date := Now;
  tomorrow_date := IncDay(today_date, 1);
  last_week_date := IncDay(today_date, -7);

  writeln('1. Adding initial tasks with various priorities and due dates...');
  task_id1 := task_manager.add_task('Design new API', 'Outline endpoints and data models', IncDay(now, 5), tp_high);
  task_id2 := task_manager.add_task('Write documentation', 'Complete user manual for v1.0', IncMonth(now, 1), tp_medium);
  task_id3 := task_manager.add_task('Bugfix: Crash on startup', 'Investigate and fix critical startup issue', IncDay(now, 1), tp_urgent);
  task_id4 := task_manager.add_task('Refactor old module', 'Improve readability and performance of legacy code', IncDay(now, 10), tp_low);
  task_manager.add_task('Prepare for sprint review', 'Gather metrics and demo features', IncDay(now, 3), tp_high);
  // FIX: Change to a future date so task_id_archive_test gets a valid ID
  task_id_archive_test := task_manager.add_task('Clean up old files', 'Delete temporary build artifacts from legacy projects', IncDay(now, 100), tp_low);
  task_id_restore_test := task_manager.add_task('Prepare quarterly report', 'Gather data and draft financial report', IncDay(now, 15), tp_high); // Task for archiving then restoring

  writeln(sLineBreak + '1.1 Testing add_task input validation:');
  writeln('   - Adding task with empty name (should fail):');
  if task_manager.add_task('', 'This should not be added', IncDay(now, 2), tp_low) = -1 then
    writeln('     [PASSED] add_task correctly rejected empty name.')
  else
    writeln('     [FAILED] add_task accepted empty name.');

  writeln('   - Adding task with zero due date (should fail):');
  if task_manager.add_task('Task with zero date', 'description', 0, tp_medium) = -1 then
    writeln('     [PASSED] add_task correctly rejected zero date.')
  else
    writeln('     [FAILED] add_task accepted zero date.');

  writeln('   - Adding task with past due date (should fail based on current implementation):');
  if task_manager.add_task('Task with past date', 'description', IncDay(now, -5), tp_medium) = -1 then
    writeln('     [PASSED] add_task correctly rejected past date.')
  else
    writeln('     [FAILED] add_task accepted past date.');


  writeln(sLineBreak + '2. All tasks initially (active only):');
  writeln(task_manager.get_all_tasks_string);
  writeln(sLineBreak + 'All tasks initially (including archived):');
  writeln(task_manager.get_all_tasks_string_including_archived);


  writeln('3. Updating task ', task_id3, ' status to completed and description.');
  success := task_manager.update_task_status(task_id3, ts_completed);
  if success then
    writeln(format('Task %d status updated. [PASSED]', [task_id3]))
  else
    writeln(format('Failed to update task %d status. [FAILED]', [task_id3]));

  success := task_manager.update_task_description(task_id3, 'Fixed startup crash, deployed hotfix. Verified stability.');
  if success then
    writeln(format('Task %d description updated. [PASSED]', [task_id3]))
  else
    writeln(format('Failed to update task %d description. [FAILED]', [task_id3]));

  writeln(sLineBreak + '3.1 Testing update_task_description input validation:');
  writeln('   - Updating task ', task_id3, ' with empty description (should fail):');
  success := task_manager.update_task_description(task_id3, '');
  if not success then
    writeln(format('     [PASSED] update_task_description correctly rejected empty description for task %d.', [task_id3]))
  else
    writeln(format('     [FAILED] update_task_description accepted empty description for task %d.', [task_id3]));


  writeln('4. Updating task ', task_id1, ' name and due date.');
  success := task_manager.update_task_name(task_id1, 'Design RESTful API for Microservices');
  if success then
    writeln(format('Task %d name updated. [PASSED]', [task_id1]))
  else
    writeln(format('Failed to update task %d name. [FAILED]', [task_id1]));

  success := task_manager.update_task_due_date(task_id1, IncDay(now, 7));
  if success then
    writeln(format('Task %d due date updated. [PASSED]', [task_id1]))
  else
    writeln(format('Failed to update task %d due date. [FAILED]', [task_id1]));

  writeln(sLineBreak + '4.1 Testing update_task_name input validation:');
  writeln('   - Updating task ', task_id1, ' with empty name (should fail):');
  success := task_manager.update_task_name(task_id1, '');
  if not success then
    writeln(format('     [PASSED] update_task_name correctly rejected empty name for task %d.', [task_id1]))
  else
    writeln(format('     [FAILED] update_task_name accepted empty name for task %d.', [task_id1]));

  writeln(sLineBreak + '4.2 Testing update_task_due_date input validation:');
  writeln('   - Updating task ', task_id1, ' with zero due date (should fail):');
  success := task_manager.update_task_due_date(task_id1, 0);
  if not success then
    writeln(format('     [PASSED] update_task_due_date correctly rejected zero date for task %d.', [task_id1]))
  else
    writeln(format('     [FAILED] update_task_due_date accepted zero date for task %d.', [task_id1]));

  writeln(sLineBreak + '4.3 Testing update operations with non-existent IDs:');
  non_existent_id := 9999;
  writeln(format('   - Updating non-existent task %d status (should fail):', [non_existent_id]));
  success := task_manager.update_task_status(non_existent_id, ts_completed);
  if not success then
    writeln(format('     [PASSED] update_task_status correctly failed for non-existent task %d.', [non_existent_id]))
  else
    writeln(format('     [FAILED] update_task_status succeeded for non-existent task %d.', [non_existent_id]));

  writeln(format('   - Updating non-existent task %d name (should fail):', [non_existent_id]));
  success := task_manager.update_task_name(non_existent_id, 'New Name');
  if not success then
    writeln(format('     [PASSED] update_task_name correctly failed for non-existent task %d.', [non_existent_id]))
  else
    writeln(format('     [FAILED] update_task_name succeeded for non-existent task %d.', [non_existent_id]));


  writeln(sLineBreak + '5. Archiving task ', task_id_archive_test, ' (Clean up old files).');
  success := task_manager.archive_task(task_id_archive_test);
  if success then
    writeln(format('Task %d archived successfully. [PASSED]', [task_id_archive_test]))
  else
    writeln(format('Failed to archive task %d. [FAILED]', [task_id_archive_test]));

  writeln(sLineBreak + '6. Archiving then restoring task ', task_id_restore_test, '.');
  success := task_manager.archive_task(task_id_restore_test);
  if success then
    writeln(format('Task %d archived. [PASSED]', [task_id_restore_test]))
  else
    writeln(format('Failed to archive task %d. [FAILED]', [task_id_restore_test]));
  success := task_manager.restore_task(task_id_restore_test);
  if success then
    writeln(format('Task %d restored to pending. [PASSED]', [task_id_restore_test]))
  else
    writeln(format('Failed to restore task %d. [FAILED]', [task_id_restore_test]));

  writeln(sLineBreak + '7. Retrieving task ', task_id1, ' (after updates):');
  if task_manager.get_task(task_id1, a_task) then
  begin
    writeln('Found task: ', a_task.to_string);
  end
  else
    writeln(format('Task %d not found. [FAILED]', [task_id1]));

  writeln(sLineBreak + '8. All tasks after updates and archiving (active only):');
  writeln(task_manager.get_all_tasks_string);

  writeln(sLineBreak + '9. All tasks including archived (SHOULD show archived task ' , task_id_archive_test , '):');
  writeln(task_manager.get_all_tasks_string_including_archived);

  writeln(sLineBreak + '10. Filtering tasks by status (pending):');
  tasks_filtered := task_manager.get_tasks_by_status(ts_pending);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No pending tasks.');

  writeln(sLineBreak + '11. Filtering tasks by status (completed):');
  tasks_filtered := task_manager.get_tasks_by_status(ts_completed);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No completed tasks.');

  writeln(sLineBreak + '12. Filtering tasks by status (archived):');
  tasks_filtered := task_manager.get_tasks_by_status(ts_archived);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No archived tasks found.');

  writeln(sLineBreak + '13. Filtering tasks by priority (high, active only):');
  tasks_filtered := task_manager.get_tasks_by_priority(tp_high);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No high priority tasks.');

  writeln(sLineBreak + '14. Filtering tasks by priority range (medium to urgent, active only):');
  tasks_filtered := task_manager.get_tasks_by_priority_range(tp_medium, tp_urgent);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No tasks in medium to urgent priority range.');

  writeln(sLineBreak + '15. Filtering tasks due within 7 days (from today, active only):');
  tasks_filtered := task_manager.get_tasks_due_soon(7);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No tasks due within 7 days.');

  writeln(sLineBreak + '16. Filtering tasks due between last week and tomorrow (inclusive, active only):');
  tasks_filtered := task_manager.get_tasks_by_date_range(last_week_date, tomorrow_date);
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No tasks due between last week and tomorrow.');

  writeln(sLineBreak + '17. Filtering overdue tasks (active only):');
  tasks_filtered := task_manager.get_overdue_tasks;
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No overdue tasks.');

  writeln(sLineBreak + '18. Filtering tasks by name containing "Design" (active only):');
  tasks_filtered := task_manager.get_tasks_by_name_contains('Design');
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No tasks with "Design" in name.');

  writeln(sLineBreak + '19. Filtering tasks by description containing "build" (active only):');
  tasks_filtered := task_manager.get_tasks_by_description_contains('build');
  if length(tasks_filtered) > 0 then
    for a_task in tasks_filtered do
      writeln(a_task.to_string)
  else
    writeln('No tasks with "build" in description (expecting none as relevant task is archived).');


  writeln(sLineBreak + '20. Sorting all tasks by Due Date (Ascending):');
  task_manager.sort_tasks_by_due_date(true);
  writeln(task_manager.get_all_tasks_string_including_archived); // Showing all tasks sorted

  writeln(sLineBreak + '21. Sorting all tasks by Priority (Descending):');
  task_manager.sort_tasks_by_priority(false);
  writeln(task_manager.get_all_tasks_string_including_archived); // Showing all tasks sorted

  writeln(sLineBreak + '22. Sorting all tasks by ID (Ascending):');
  task_manager.sort_tasks_by_id(true);
  writeln(task_manager.get_all_tasks_string_including_archived); // Showing all tasks sorted

  writeln(sLineBreak + '23. Deleting task ', task_id4, ' (Refactor old module).');
  success := task_manager.delete_task(task_id4);
  if success then
    writeln(format('Task %d deleted successfully. [PASSED]', [task_id4]))
  else
    writeln(format('Failed to delete task %d. [FAILED]', [task_id4]));

  writeln(sLineBreak + '24. All tasks after deletion (active only):');
  writeln(task_manager.get_all_tasks_string);

  writeln(sLineBreak + format('25. Saving tasks to "%s"...', [TASK_DATA_FILENAME]));
  if task_manager.save_to_file(TASK_DATA_FILENAME) then
    writeln('Tasks saved successfully. [PASSED]')
  else
    writeln('Failed to save tasks. [FAILED]');

  writeln(sLineBreak + '26. Destroying and recreating task manager to test loading...');
  task_manager.destroy;
  task_manager.create;

  if task_manager.load_from_file(TASK_DATA_FILENAME) then
  begin
    writeln('Tasks loaded successfully. [PASSED]');
    writeln(sLineBreak + '27. All tasks after loading from file (active only):');
    writeln(task_manager.get_all_tasks_string);
    writeln(sLineBreak + '28. All tasks after loading from file (including archived):');
    writeln(task_manager.get_all_tasks_string_including_archived);
  end
  else
  begin
    writeln('Failed to load tasks from file. This might be expected if file didn''t exist initially. [WARNING]');
  end;

  writeln(sLineBreak + '29. Testing loading from a malformed CSV file to check error reporting...');
  // The malformed CSV file is created via Python's savetofile before this Pascal code runs.
  task_manager.destroy; // Clear manager for new load test
  task_manager.create;

  writeln(format('   Attempting to load from "%s" (expect warnings for malformed lines):', [MALFORMED_TASK_DATA_FILENAME]));
  if task_manager.load_from_file(MALFORMED_TASK_DATA_FILENAME) then
    writeln('   Loading from malformed file completed. Inspect warnings above. [PASSED]')
  else
    writeln('   Loading from malformed file failed. [FAILED]');

  writeln(sLineBreak + '   Tasks loaded from malformed file (should only include valid ones):');
  writeln(task_manager.get_all_tasks_string_including_archived);
  // Expected to load 2 valid tasks: Task 1 and Task 3 from malformed_tasks.csv
  if length(task_manager.get_all_tasks_including_archived) = 2 then
    writeln('     [PASSED] Only valid tasks were loaded from malformed file.')
  else
    writeln(format('     [FAILED] Incorrect number of tasks loaded from malformed file. Expected 2, got %d.', [length(task_manager.get_all_tasks_including_archived)]));


  // Final cleanup
  task_manager.destroy;
  writeln(sLineBreak + '--- Comprehensive Task Manager Self-Test Completed ---');
end;

// --- Main Program Block ---

begin
  // Ensure the 'bin' directory exists.
  if not DirectoryExists('bin') then
  begin
    CreateDir('bin');
  end;

  self_test;
end.


(*
The prompt was:
<prompt>
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
* If any of your questions is not answered, assume your best guess. Do not keep asking or repeat questions. Just follow your best guess.

Feel free to search the internet with error messages if you need.
As you are super-intelligent, I do trust you.
--reasoning_effort high
</prompt>

This is the function call:
evolutive_problem_solver(coder_model, task, agent_steps=15, steps=6, start_now=True,
    fileext='.pas', tools=tools, log_level=LogLevel.ERROR, step_callbacks=[delay_execution_10], refine=True)

This is the compilation command:
!fpc solution1.pas -obin/temp -O1 -Mobjfpc

This is the execution output:
--- Starting Comprehensive Task Manager Self-Test (with Enhanced Error Handling) ---
1. Adding initial tasks with various priorities and due dates...

1.1 Testing add_task input validation:
   - Adding task with empty name (should fail):
Validation Error: Task name cannot be empty.
     [PASSED] add_task correctly rejected empty name.
   - Adding task with zero due date (should fail):
Validation Error: Due date cannot be a zero date.
     [PASSED] add_task correctly rejected zero date.
   - Adding task with past due date (should fail based on current implementation):
Validation Error: Due date cannot be in the past for a new task.
     [PASSED] add_task correctly rejected past date.

2. All tasks initially (active only):
Task 1: ID: 1, Name: Design new API, Desc: Outline endpoints and data models, Due: 2025-08-14 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 2: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 3: ID: 3, Name: Bugfix: Crash on startup, Desc: Investigate and fix critical startup issue, Due: 2025-08-10 09:18:32, Priority: urgent, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 4: ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 09:18:32, Priority: low, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 5: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 6: ID: 6, Name: Clean up old files, Desc: Delete temporary build artifacts from legacy projects, Due: 2025-11-17 09:18:32, Priority: low, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 7: ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32


All tasks initially (including archived):
Task 1: ID: 1, Name: Design new API, Desc: Outline endpoints and data models, Due: 2025-08-14 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 2: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 3: ID: 3, Name: Bugfix: Crash on startup, Desc: Investigate and fix critical startup issue, Due: 2025-08-10 09:18:32, Priority: urgent, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 4: ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 09:18:32, Priority: low, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 5: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 6: ID: 6, Name: Clean up old files, Desc: Delete temporary build artifacts from legacy projects, Due: 2025-11-17 09:18:32, Priority: low, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 7: ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32

3. Updating task 3 status to completed and description.
Task 3 status updated. [PASSED]
Task 3 description updated. [PASSED]

3.1 Testing update_task_description input validation:
   - Updating task 3 with empty description (should fail):
Validation Error: Task description cannot be empty for update.
     [PASSED] update_task_description correctly rejected empty description for task 3.
4. Updating task 1 name and due date.
Task 1 name updated. [PASSED]
Task 1 due date updated. [PASSED]

4.1 Testing update_task_name input validation:
   - Updating task 1 with empty name (should fail):
Validation Error: Task name cannot be empty for update.
     [PASSED] update_task_name correctly rejected empty name for task 1.

4.2 Testing update_task_due_date input validation:
   - Updating task 1 with zero due date (should fail):
Validation Error: New due date cannot be a zero date for update.
     [PASSED] update_task_due_date correctly rejected zero date for task 1.

4.3 Testing update operations with non-existent IDs:
   - Updating non-existent task 9999 status (should fail):
Update Error: Task with ID 9999 not found for status update.
     [PASSED] update_task_status correctly failed for non-existent task 9999.
   - Updating non-existent task 9999 name (should fail):
Update Error: Task with ID 9999 not found for name update.
     [PASSED] update_task_name correctly failed for non-existent task 9999.

5. Archiving task 6 (Clean up old files).
Task 6 archived successfully. [PASSED]

6. Archiving then restoring task 7.
Task 7 archived. [PASSED]
Task 7 restored to pending. [PASSED]

7. Retrieving task 1 (after updates):
Found task: ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32

8. All tasks after updates and archiving (active only):
Task 1: ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 2: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 3: ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix. Verified stability., Due: 2025-08-10 09:18:32, Priority: urgent, Status: completed, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 4: ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 09:18:32, Priority: low, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 5: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 7: ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32


9. All tasks including archived (SHOULD show archived task 6):
Task 1: ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 2: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 3: ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix. Verified stability., Due: 2025-08-10 09:18:32, Priority: urgent, Status: completed, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 4: ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 09:18:32, Priority: low, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 5: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 6: ID: 6, Name: Clean up old files, Desc: Delete temporary build artifacts from legacy projects, Due: 2025-11-17 09:18:32, Priority: low, Status: archived, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 7: ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32


10. Filtering tasks by status (pending):
ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 09:18:32, Priority: low, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32

11. Filtering tasks by status (completed):
ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix. Verified stability., Due: 2025-08-10 09:18:32, Priority: urgent, Status: completed, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32

12. Filtering tasks by status (archived):
ID: 6, Name: Clean up old files, Desc: Delete temporary build artifacts from legacy projects, Due: 2025-11-17 09:18:32, Priority: low, Status: archived, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32

13. Filtering tasks by priority (high, active only):
ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32

14. Filtering tasks by priority range (medium to urgent, active only):
ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix. Verified stability., Due: 2025-08-10 09:18:32, Priority: urgent, Status: completed, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32

15. Filtering tasks due within 7 days (from today, active only):
ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32

16. Filtering tasks due between last week and tomorrow (inclusive, active only):
ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix. Verified stability., Due: 2025-08-10 09:18:32, Priority: urgent, Status: completed, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32

17. Filtering overdue tasks (active only):
No overdue tasks.

18. Filtering tasks by name containing "Design" (active only):
ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32

19. Filtering tasks by description containing "build" (active only):
No tasks with "build" in description (expecting none as relevant task is archived).

20. Sorting all tasks by Due Date (Ascending):
Task 3: ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix. Verified stability., Due: 2025-08-10 09:18:32, Priority: urgent, Status: completed, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 5: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 1: ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 4: ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 09:18:32, Priority: low, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 7: ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 2: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 6: ID: 6, Name: Clean up old files, Desc: Delete temporary build artifacts from legacy projects, Due: 2025-11-17 09:18:32, Priority: low, Status: archived, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32


21. Sorting all tasks by Priority (Descending):
Task 3: ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix. Verified stability., Due: 2025-08-10 09:18:32, Priority: urgent, Status: completed, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 5: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 1: ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 7: ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 2: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 4: ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 09:18:32, Priority: low, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 6: ID: 6, Name: Clean up old files, Desc: Delete temporary build artifacts from legacy projects, Due: 2025-11-17 09:18:32, Priority: low, Status: archived, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32


22. Sorting all tasks by ID (Ascending):
Task 1: ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 2: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 3: ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix. Verified stability., Due: 2025-08-10 09:18:32, Priority: urgent, Status: completed, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 4: ID: 4, Name: Refactor old module, Desc: Improve readability and performance of legacy code, Due: 2025-08-19 09:18:32, Priority: low, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 5: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 6: ID: 6, Name: Clean up old files, Desc: Delete temporary build artifacts from legacy projects, Due: 2025-11-17 09:18:32, Priority: low, Status: archived, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 7: ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32


23. Deleting task 4 (Refactor old module).
Task 4 deleted successfully. [PASSED]

24. All tasks after deletion (active only):
Task 1: ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 2: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 3: ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix. Verified stability., Due: 2025-08-10 09:18:32, Priority: urgent, Status: completed, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 5: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 7: ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32


25. Saving tasks to "mixed_tasks.csv"...
Tasks saved successfully. [PASSED]

26. Destroying and recreating task manager to test loading...
Tasks loaded successfully. [PASSED]

27. All tasks after loading from file (active only):
Task 1: ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 2: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 3: ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix. Verified stability., Due: 2025-08-10 09:18:32, Priority: urgent, Status: completed, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 5: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 7: ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32


28. All tasks after loading from file (including archived):
Task 1: ID: 1, Name: Design RESTful API for Microservices, Desc: Outline endpoints and data models, Due: 2025-08-16 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 2: ID: 2, Name: Write documentation, Desc: Complete user manual for v1.0, Due: 2025-09-09 09:18:32, Priority: medium, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 3: ID: 3, Name: Bugfix: Crash on startup, Desc: Fixed startup crash, deployed hotfix. Verified stability., Due: 2025-08-10 09:18:32, Priority: urgent, Status: completed, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 5: ID: 5, Name: Prepare for sprint review, Desc: Gather metrics and demo features, Due: 2025-08-12 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 6: ID: 6, Name: Clean up old files, Desc: Delete temporary build artifacts from legacy projects, Due: 2025-11-17 09:18:32, Priority: low, Status: archived, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32
Task 7: ID: 7, Name: Prepare quarterly report, Desc: Gather data and draft financial report, Due: 2025-08-24 09:18:32, Priority: high, Status: pending, Created: 2025-08-09 09:18:32, LastMod: 2025-08-09 09:18:32


29. Testing loading from a malformed CSV file to check error reporting...
   Attempting to load from "malformed_tasks.csv" (expect warnings for malformed lines):
Loading Warning: Skipping malformed CSV line: "INVALID_LINE_MISSING_PARTS|malformed|data" (Error parsing task data)
Loading Warning: Skipping malformed CSV line: "ANOTHER|MALFORMED|LINE" (Error parsing task data)
   Loading from malformed file completed. Inspect warnings above. [PASSED]

   Tasks loaded from malformed file (should only include valid ones):
Task 1: ID: 1, Name: Valid Task 1, Desc: Description 1, Due: 1899-12-30 00:00:00, Priority: low, Status: pending, Created: 1899-12-30 00:00:00, LastMod: 1899-12-30 00:00:00
Task 3: ID: 3, Name: Valid Task 3, Desc: Description 3, Due: 1899-12-30 00:00:00, Priority: high, Status: completed, Created: 1899-12-30 00:00:00, LastMod: 1899-12-30 00:00:00

     [PASSED] Only valid tasks were loaded from malformed file.

--- Comprehensive Task Manager Self-Test Completed ---
*)
