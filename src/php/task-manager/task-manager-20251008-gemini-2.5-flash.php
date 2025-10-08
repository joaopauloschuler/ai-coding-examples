<?php
// created by https://github.com/joaopauloschuler/beyond-python-smolagents
// V20.0: Reverse Dependency Reporting + Skip Command + Dependency Tracking + Enhanced Tag Filtering
define('USER_TIMEZONE', 'America/Los_Angeles'); 
// Set PHP's default timezone to the user's preferred timezone for parsing relative dates (strtotime)
date_default_timezone_set(USER_TIMEZONE); 

// solution.php: Super Hybrid Task Manager (V20.0 - Reverse Dependency)
// This version adds the ability to see which tasks depend on the current task.

/**
 * Task Class
 * Represents a single unit of work with status, priority, and recurrence tracking.
 */
class Task {
    public $id;
    public $title;
    public $description;
    public $due_date; // DateTime object or null (Stored in UTC)
    public $priority; 
    public $status; 
    public $created_at; // DateTime object (Stored in UTC)
    public $is_recurring;
    public $recurrence_pattern;     // ISO 8601 duration string (e.g., P3D, P2W)
    public $recurrence_limit_count; // Max repetitions remaining (integer or null)
    public $recurrence_end_date;    // End date for recurrence (DateTime object, stored in UTC midnight)
    public $tags;                   // V15.2: Array of strings
    public $dependencies;           // V16.0: Array of integer Task IDs

    /**
     * Helper to convert date strings (from persistence) into normalized DateTime objects.
     * Requires UTC timezone object for correct interpretation of stored strings.
     */
    private static function convertDateToDateTime(?string $dateString, DateTimeZone $utcTimezone): ?DateTime {
        if (empty($dateString)) {
            return null;
        }
        
        // 1. Try Y-m-d H:i:s (Stored in UTC)
        // Use the passed UTC timezone object for interpretation
        $d = DateTime::createFromFormat('Y-m-d H:i:s', $dateString, $utcTimezone);
        if ($d && $d->format('Y-m-d H:i:s') === $dateString) return $d;
        
        // 2. Try Y-m-d (Legacy format, stored in UTC midnight)
        $d = DateTime::createFromFormat('Y-m-d', $dateString, $utcTimezone);
        if ($d && $d->format('Y-m-d') === $dateString) return $d->setTime(0, 0, 0); 
        
        return null;
    }

    /**
     * @param int $id
     * @param string $title
     * @param string $description
     * @param string $priority
     * @param DateTime|null $due_date (Must be UTC)
     * @param bool $is_recurring
     * @param string|null $recurrence_pattern
     * @param int|null $limit_count
     * @param DateTime|null $end_date (Must be UTC)
     * @param array $tags // V15.2
     * @param array $dependencies // V16.0
     */
    public function __construct($id, $title, $description, $priority = 'Medium', ?DateTime $due_date = null, $is_recurring = false, $recurrence_pattern = null, ?int $limit_count = null, ?DateTime $end_date = null, array $tags = [], array $dependencies = []) { // V16.0: Added $dependencies
        $this->id = $id;
        $this->title = $title;
        $this->description = $description;
        $this->due_date = $due_date;
        
        // Ensure created_at is initialized in UTC
        $utc = new DateTimeZone('UTC');
        $this->created_at = new DateTime('now', $utc); 
        
        $this->priority = $this->normalizePriority($priority);
        $this->is_recurring = (bool)$is_recurring;
        $this->recurrence_pattern = $this->normalizeRecurrence($recurrence_pattern);
        $this->recurrence_limit_count = $limit_count;
        $this->recurrence_end_date = $end_date;
        $this->tags = $this->normalizeTags($tags); // V15.2
        $this->dependencies = $this->normalizeDependencies($dependencies); // V16.0
        $this->status = 'pending';
        $this->updateStatus();
    }

    /**
     * Normalizes priority string to 'High', 'Medium', or 'Low'.
     */
    public function normalizePriority(string $priority): string {
        $priority = ucfirst(strtolower($priority));
        return in_array($priority, ['High', 'Medium', 'Low']) ? $priority : 'Medium';
    }
    
    /**
     * Normalizes recurrence pattern.
     */
    public function normalizeRecurrence(?string $pattern): ?string {
        if (!$pattern) {
            return null;
        }
        $pattern = strtoupper($pattern);
        
        // Map simple keywords to ISO 8601 duration format (P1D, P1W, P1M)
        switch ($pattern) {
            case 'DAILY':
                return 'P1D';
            case 'WEEKLY':
                return 'P1W';
            case 'MONTHLY':
                return 'P1M';
        }
        
        // Check if it looks like a valid ISO 8601 duration (starts with P)
        if (strpos($pattern, 'P') === 0) {
            // Attempt to create a DateInterval to validate the format
            try {
                new DateInterval($pattern);
                return $pattern;
            } catch (Exception $e) {
                // Invalid ISO format
            }
        }
        
        return null;
    }
    
    /**
     * V15.2: Normalizes and cleans up tags.
     */
    public function normalizeTags(array $tags): array {
        // Filter out empty strings, trim whitespace, convert to lowercase, and ensure uniqueness
        return array_unique(array_filter(array_map('strtolower', array_map('trim', $tags))));
    }
    
    /**
     * V16.0: Normalizes and cleans up dependencies (ensures array of unique positive integers).
     */
    public function normalizeDependencies(array $dependencies): array {
        // Filter out non-numeric/non-positive values, cast to int, and ensure uniqueness
        $normalized = array_filter($dependencies, fn($id) => is_numeric($id) && $id > 0);
        return array_values(array_unique(array_map('intval', $normalized)));
    }


    /**
     * Updates the task status based on the due date, unless already completed or archived.
     * Compares against current time, ensuring both are in UTC.
     */
    public function updateStatus() {
        if ($this->status === 'completed' || $this->status === 'archived') {
            return;
        }
        
        if ($this->due_date instanceof DateTime) {
            // Compare against current time, ensuring both are in UTC.
            $utc = new DateTimeZone('UTC');
            $now = new DateTime('now', $utc); 
            
            if ($this->due_date < $now) {
                $this->status = 'overdue';
            } else {
                $this->status = 'pending';
            }
        } else {
            $this->status = 'pending';
        }
    }

    public function markComplete() {
        $this->status = 'completed';
    }
    
    public function markArchived() {
        $this->status = 'archived';
    }

    /**
     * Converts the Task object to an array, converting DateTime objects back to strings (in UTC).
     */
    public function toArray() {
        $data = get_object_vars($this); 
        
        // Convert DateTime objects to strings for persistence (Y-m-d H:i:s UTC format)
        // Since the objects are already in UTC, we just format them.
        $data['due_date'] = $this->due_date instanceof DateTime ? $this->due_date->format('Y-m-d H:i:s') : null;
        $data['created_at'] = $this->created_at instanceof DateTime ? $this->created_at->format('Y-m-d H:i:s') : date('Y-m-d H:i:s');
        
        // Recurrence end date (Date only format Y-m-d)
        $data['recurrence_end_date'] = $this->recurrence_end_date instanceof DateTime ? $this->recurrence_end_date->format('Y-m-d') : null;
        
        $data['tags'] = $this->tags; // V15.2
        $data['dependencies'] = $this->dependencies; // V16.0
        
        return $data;
    }

    /**
     * Creates a Task object from a persistent array, converting date strings to DateTime objects.
     * Requires UTC timezone object to interpret stored strings correctly.
     */
    public static function fromArray(array $data, DateTimeZone $utcTimezone): Task {
        
        // Prepare DateTime objects from stored strings, interpreting them as UTC
        $due_date_dt = self::convertDateToDateTime($data['due_date'] ?? null, $utcTimezone);
        $created_at_dt = self::convertDateToDateTime($data['created_at'] ?? null, $utcTimezone) ?? new DateTime('now', $utcTimezone);
        
        // Load recurrence end date
        $recurrence_end_date_dt = self::convertDateToDateTime($data['recurrence_end_date'] ?? null, $utcTimezone);
        
        // V15.2 Load tags
        $tags = $data['tags'] ?? [];
        if (!is_array($tags)) $tags = [];
        
        // V16.0 Load dependencies
        $dependencies = $data['dependencies'] ?? [];
        if (!is_array($dependencies)) $dependencies = [];
        
        // Create a temporary Task object using the constructor
        $task = new Task(
            $data['id'] ?? 0,
            $data['title'] ?? 'Unknown Title',
            $data['description'] ?? 'No Description',
            $data['priority'] ?? 'Medium',
            $due_date_dt, // Pass UTC DateTime object
            $data['is_recurring'] ?? false,
            $data['recurrence_pattern'] ?? null,
            $data['recurrence_limit_count'] ?? null,
            $recurrence_end_date_dt,
            $tags, // V15.2
            $dependencies // V16.0
        );
        
        // Restore status and ensure created_at is set correctly (must be UTC)
        $task->status = $data['status'] ?? 'pending';
        $task->created_at = $created_at_dt;
        
        $task->updateStatus(); // Re-check overdue status upon load
        return $task;
    }
}

/**
 * TaskManager Class
 * Handles task persistence, management, filtering, and CLI interaction.
 */
class TaskManager {
    private $storageFile = 'tasks.json';
    private $tasks = [];
    private $userTimezone; // V15.1
    private $utcTimezone;  // V15.1

    public function __construct($storageFile = 'tasks.json') {
        $this->storageFile = $storageFile;
        
        // V15.1: Initialize Timezones
        $this->userTimezone = new DateTimeZone(USER_TIMEZONE);
        $this->utcTimezone = new DateTimeZone('UTC');
        
        $this->loadTasks();
        $this->processRecurrenceQueue(); // Handle overdue recurring tasks immediately on load
    }

    // --- Persistence ---
    private function loadTasks() {
        if (!file_exists($this->storageFile)) {
            $this->tasks = [];
            return;
        }

        $json = @file_get_contents($this->storageFile);
        
        if ($json === false) {
            echo "\033[31m[ERROR] Could not read storage file: {$this->storageFile}\033[0m\n";
            $this->tasks = [];
            return;
        }
        
        if (trim($json) === '') {
            $this->tasks = [];
            return;
        }

        $data = json_decode($json, true);
        
        if (json_last_error() !== JSON_ERROR_NONE) {
            $error_message = json_last_error_msg();
            echo "\033[31m[ERROR] Corrupted JSON data detected in {$this->storageFile}. Error: {$error_message}\003[0m\n";
            
            // Backup the corrupted file using a timestamp
            $backupFile = $this->storageFile . '.bak_' . date('YmdHis');
            if (rename($this->storageFile, $backupFile)) {
                echo "\033[33m[WARNING] Corrupted file backed up to {$backupFile}. Task list initialized empty.\033[0m\n";
            } else {
                echo "\033[31m[CRITICAL] Failed to backup corrupted file. Task list initialized empty.\033[0m\n";
            }
            
            $this->tasks = [];
            return;
        }

        if (is_array($data)) {
            // V15.1: Ensure tasks are loaded as Task objects, passing UTC timezone
            $utcTimezone = $this->utcTimezone;
            $this->tasks = array_map(fn($taskData) => Task::fromArray($taskData, $utcTimezone), $data);
        } else {
            echo "\033[31m[ERROR] JSON decoded successfully but root element is not an array. Task list initialized empty.\003[0m\n";
            $this->tasks = [];
        }
    }

    private function saveTasks() {
        $data = array_map(fn(Task $task) => $task->toArray(), $this->tasks);
        $result = @file_put_contents($this->storageFile, json_encode($data, JSON_PRETTY_PRINT));
        
        // Critical write error handling
        if ($result === false) {
            $error_message = error_get_last()['message'] ?? 'Unknown file write error.';
            echo "\033[31m[CRITICAL ERROR] Failed to save tasks to {$this->storageFile}. Check file permissions or disk space. Error: {$error_message}\003[0m\n";
        }
    }

    private function generateUniqueId() {
        $maxId = 0;
        foreach ($this->tasks as $task) {
            if ($task->id > $maxId) {
                $maxId = $task->id;
            }
        }
        return $maxId + 1;
    }
    
    // --- Utility Methods (Date/Time Validation, Colors, Dependency Checks) ---
    
    /**
     * V15.1: Converts a stored UTC DateTime object to the user's preferred timezone for display.
     */
    private function convertToUserTimezone(DateTime $dt): DateTime {
        // Clone to avoid modifying the original UTC object stored in the Task
        $dt = clone $dt; 
        return $dt->setTimezone($this->userTimezone);
    }
    
    /**
     * V15.1: Flexible date/time validation. Parses input assuming USER_TIMEZONE, 
     * then converts the resulting DateTime object to UTC before returning.
     * @throws Exception if date/time is invalid.
     */
    private function validateDateTime(?string $dateTime): ?DateTime {
        if ($dateTime === null || empty($dateTime)) {
            return null;
        }

        $userTZ = $this->userTimezone;
        $utcTZ = $this->utcTimezone;
        $d = null;

        // 1. Try strict YYYY-MM-DD HH:MM:SS (Parsed in USER_TIMEZONE)
        $d = DateTime::createFromFormat('Y-m-d H:i:s', $dateTime, $userTZ);
        if ($d && $d->format('Y-m-d H:i:s') === $dateTime) {
            // Success
        } else {
            // 2. Try strict YYYY-MM-DD (Default time to midnight in USER_TIMEZONE)
            $d = DateTime::createFromFormat('Y-m-d', $dateTime, $userTZ);
            if ($d && $d->format('Y-m-d') === $dateTime) {
                $d->setTime(0, 0, 0); 
            } else {
                // 3. Try flexible relative date parsing (e.g., "tomorrow", "+1 week")
                // strtotime uses the default PHP timezone (USER_TIMEZONE)
                $timestamp = strtotime($dateTime);
                
                if ($timestamp !== false) {
                    // Create DateTime object using the timestamp, setting its timezone to USER_TIMEZONE
                    $parsed_date = new DateTime("@$timestamp");
                    $parsed_date->setTimezone($userTZ);
                    
                    // Prevent simple numbers (like '123') that might be misinterpreted as old timestamps 
                    if (!is_numeric($dateTime) || $timestamp >= time() - (365 * 24 * 3600)) {
                         $d = $parsed_date;
                    }
                }
            }
        }
        
        if ($d instanceof DateTime) {
            // V15.1: Convert the resulting DateTime object from USER_TIMEZONE to UTC for storage
            $d->setTimezone($utcTZ);
            return $d;
        }

        throw new Exception("Invalid date/time format: '{$dateTime}'. Please use YYYY-MM-DD, YYYY-MM-DD HH:MM:SS, or a relative string (e.g., 'tomorrow 9am', '+1 week').");
    }

    private function getColorForStatus(string $status): string {
        if ($status === 'completed') return "\033[32m"; // Green
        if ($status === 'overdue') return "\033[31m"; // Red
        if ($status === 'archived') return "\033[90m"; // Light Gray
        return "\033[33m"; // Yellow (Pending default)
    }

    private function getColorForPriority(string $priority): string {
        switch ($priority) {
            case 'High': return "\033[31m"; // Red
            case 'Medium': return "\033[34m"; // Blue
            case 'Low': return "\033[36m"; // Cyan
            default: return "\033[0m";
        }
    }
    
    /**
     * V16.0: Checks if all dependencies for a given task are completed.
     * Returns an array of uncompleted dependency IDs, or an empty array if all are complete.
     */
    private function checkDependenciesComplete(Task $task): array {
        $uncompleted = [];
        foreach ($task->dependencies as $depId) {
            $depTask = $this->getTask($depId);
            
            // If dependency doesn't exist or is not completed
            if (!$depTask || $depTask->status !== 'completed') {
                $uncompleted[] = $depId;
            }
        }
        return $uncompleted;
    }
    
    /**
     * V16.0: Recursive check for circular dependencies.
     * @param int $currentId The ID of the task being checked (the starting point).
     * @param array $dependencies The list of potential dependencies to check.
     * @param array $path The current path of dependencies checked so far.
     * @throws Exception if a circular dependency is detected.
     */
    private function detectCircularDependency(int $currentId, array $dependencies, array $path = []): void {
        $path[] = $currentId;
        
        foreach ($dependencies as $depId) {
            // Check if the dependency ID is already in the path
            if (in_array($depId, $path)) {
                $cycle = implode(' -> ', array_map(fn($id) => "#$id", array_slice($path, array_search($depId, $path))));
                $cycle .= " -> #$depId";
                throw new Exception("Circular dependency detected: {$cycle}");
            }
            
            $depTask = $this->getTask($depId);
            
            // If the dependency task exists and has its own dependencies, check them recursively
            if ($depTask && !empty($depTask->dependencies)) {
                $this->detectCircularDependency($depId, $depTask->dependencies, $path);
            }
        }
    }
    
    /**
     * V20.0: Finds all tasks that depend on the given task ID.
     * Returns an array of Task objects. Only returns tasks that are not completed or archived.
     */
    private function getDependentTasks(int $id): array {
        $dependents = [];
        foreach ($this->tasks as $task) {
            if (in_array($id, $task->dependencies)) {
                // Ensure we only return active tasks or tasks that are not archived/completed
                if ($task->status !== 'archived' && $task->status !== 'completed') {
                    $dependents[] = $task;
                }
            }
        }
        return $dependents;
    }


    // --- Recurrence Logic (V11.0: Time Precision + Limits) ---

    /**
     * Calculates the next due date/time based on the current date/time and pattern.
     * Ensures calculation is performed in UTC context.
     */
    private function calculateNextDueDate(string $currentDueDate, string $pattern): string {
        try {
            // V15.1: Ensure DateTime is created in UTC context for calculation
            $utc = new DateTimeZone('UTC');
            $date = new DateTime($currentDueDate, $utc);
        } catch (Exception $e) {
            throw new Exception("Invalid date format for calculation: {$currentDueDate}");
        }
        
        try {
            $interval = new DateInterval($pattern);
        } catch (Exception $e) {
            throw new Exception("Invalid recurrence pattern (must be ISO 8601 duration): {$pattern}");
        }

        $date->add($interval);
        return $date->format('Y-m-d H:i:s'); 
    }

    /**
     * Generates the next instance of a recurring task, incorporating V10.0 limits.
     */
    private function generateNextRecurrence(Task $oldTask, string $statusToSet): ?Task {
        if (!$oldTask->is_recurring || !$oldTask->recurrence_pattern || !($oldTask->due_date instanceof DateTime)) {
            return null;
        }

        try {
            // Get the date/time string from the DateTime object for calculation
            $currentDateStr = $oldTask->due_date->format('Y-m-d H:i:s');
            $nextDueDateStr = $this->calculateNextDueDate($currentDateStr, $oldTask->recurrence_pattern);
        } catch (Exception $e) {
            echo "\033[31m[ERROR] Failed to calculate next due date for task #{$oldTask->id}: {$e->getMessage()}\033[0m\n";
            return null;
        }
        
        // V15.1 FIX: Manually create the UTC DateTime object for the next due date
        $utc = $this->utcTimezone;
        $nextDueDateDT = DateTime::createFromFormat('Y-m-d H:i:s', $nextDueDateStr, $utc);
        if (!$nextDueDateDT) {
            echo "\033[31m[ERROR] Internal error: Failed to parse calculated UTC date string.\033[0m\n";
            return null; 
        }
        
        // --- V11.0 Limit Check 1: End Date ---
        if ($oldTask->recurrence_end_date instanceof DateTime) {
            // Check if the next due date is strictly AFTER the recurrence end date (both are UTC)
            if ($nextDueDateDT > $oldTask->recurrence_end_date) {
                // Limit reached: Archive the old task and DO NOT generate a new one.
                $oldTask->status = $statusToSet;
                $oldTask->is_recurring = false;
                $oldTask->recurrence_pattern = null;
                $oldTask->recurrence_limit_count = null;
                
                // Convert end date to user TZ for display in message
                $displayEndDate = $this->convertToUserTimezone($oldTask->recurrence_end_date);
                $endDateStr = $displayEndDate->format('Y-m-d');
                $oldTask->recurrence_end_date = null;
                
                echo "\033[35m[RECURRENCE LIMIT] Task #{$oldTask->id} marked {$statusToSet}. Recurrence stopped by End Date limit ({$endDateStr}).\033[0m\n";
                return null; 
            }
        }

        // --- V11.0 Limit Check 2: Count ---
        $next_limit_count = $oldTask->recurrence_limit_count;
        $new_task_is_recurring = true;
        
        if (is_int($oldTask->recurrence_limit_count) && $oldTask->recurrence_limit_count > 0) {
            $next_limit_count = $oldTask->recurrence_limit_count - 1;
            
            if ($next_limit_count === 0) {
                $new_task_is_recurring = false;
            }
        }

        // 1. Create the new task using the internal helper, passing DateTime objects directly
        $newTask = $this->_createTask(
            $oldTask->title,
            $oldTask->description,
            $oldTask->priority,
            $nextDueDateDT, // Pass validated UTC DateTime object
            $new_task_is_recurring, 
            $oldTask->recurrence_pattern,
            $new_task_is_recurring ? $next_limit_count : null,
            $oldTask->recurrence_end_date, // Pass existing UTC DateTime object
            $oldTask->tags, // V15.2: Inherit tags
            $oldTask->dependencies // V16.0: Inherit dependencies
        );
        
        // 2. Update the old task's status and remove recurrence flags (it's now a single instance)
        $oldTask->status = $statusToSet;
        $oldTask->is_recurring = false;
        $oldTask->recurrence_pattern = null;
        $oldTask->recurrence_limit_count = null; 
        $oldTask->recurrence_end_date = null;     
        
        // Convert next due date to user TZ for display in message
        $displayNextDueDate = $this->convertToUserTimezone($newTask->due_date);
        $displayNextDueDateStr = $displayNextDueDate->format('Y-m-d H:i:s');
        
        echo "\033[35m[RECURRENCE] Generated new task #{$newTask->id} (Due: {$displayNextDueDateStr} {$this->userTimezone->getName()}) from old task #{$oldTask->id} (Marked {$statusToSet}).\033[0m\n";

        return $newTask;
    }
    
    /**
     * Checks for overdue recurring tasks and generates their next instance.
     */
    private function processRecurrenceQueue() {
        $tasksModified = false;
        $tasksToProcess = [];

        // Identify overdue recurring tasks
        foreach ($this->tasks as $task) {
            $task->updateStatus(); // Ensure status is current (uses UTC comparison)
            if ($task->is_recurring && $task->status === 'overdue') {
                $tasksToProcess[] = $task;
            }
        }

        if (empty($tasksToProcess)) {
            return;
        }

        foreach ($tasksToProcess as $task) {
            // Generate the next instance. If a limit is reached, this returns null.
            $result = $this->generateNextRecurrence($task, 'archived');
            if ($result !== null) {
                $tasksModified = true;
            }
        }
        
        // If any tasks were modified (new tasks added, old ones archived), save the state.
        if ($tasksModified) {
            $this->saveTasks();
        }
    }


    // --- CRUD Operations ---

    // Private helper to handle task creation from validated UTC DateTime objects
    private function _createTask(string $title, string $description, string $priority, ?DateTime $validatedDueDateDT, bool $isRecurring, ?string $recurrencePattern, ?int $limitCount, ?DateTime $validatedEndDateDT, array $tags = [], array $dependencies = []): Task { // V16.0: Added dependencies
        
        $id = $this->generateUniqueId();
        
        // V16.0: Check for circular dependencies before adding to the list
        if (!empty($dependencies)) {
            // Check if any dependency path leads back to the ID we are about to assign.
            try {
                $this->detectCircularDependency($id, $dependencies);
            } catch (Exception $e) {
                // Re-throw the exception with a more user-friendly message
                throw new Exception("Cannot create task: " . $e->getMessage());
            }
        }
        
        // V16.0: Pass dependencies to Task constructor
        $task = new Task($id, $title, $description, $priority, $validatedDueDateDT, $isRecurring, $recurrencePattern, $limitCount, $validatedEndDateDT, $tags, $dependencies); 
        $this->tasks[] = $task;
        return $task;
    }

    public function addTask(string $title, string $description, string $priority = 'Medium', string $dueDate = null, bool $isRecurring = false, string $recurrencePattern = null, ?int $limitCount = null, string $recurrenceEndDate = null, array $tags = [], array $dependencies = []): Task { // V16.0: Added dependencies
        // Strict Date/Time Validation (returns DateTime object in UTC)
        $validatedDueDateDT = $this->validateDateTime($dueDate); 
        
        // Validate Recurrence End Date (returns DateTime object in UTC)
        $validatedEndDateDT = $this->validateDateTime($recurrenceEndDate);

        // Basic sanity check
        if ($limitCount !== null && $limitCount <= 0) {
            throw new Exception("Recurrence limit count must be a positive integer.");
        }
        
        $task = $this->_createTask($title, $description, $priority, $validatedDueDateDT, $isRecurring, $recurrencePattern, $limitCount, $validatedEndDateDT, $tags, $dependencies); // V16.0: Passed dependencies
        $this->saveTasks();
        return $task;
    }

    public function getTask(int $id): ?Task {
        foreach ($this->tasks as $task) {
            if ($task->id == $id) {
                return $task;
            }
        }
        return null;
    }

    public function markTaskComplete(int $id): bool {
        $task = $this->getTask($id);
        if ($task) {
            
            // V16.0: Dependency Check
            $uncompleted = $this->checkDependenciesComplete($task);
            if (!empty($uncompleted)) {
                $list = implode(', ', array_map(fn($depId) => "#$depId", $uncompleted));
                echo "\033[31m[ERROR] Cannot complete Task #{$id} ('{$task->title}'). The following dependencies must be completed first: {$list}\033[0m\n";
                return false;
            }
            
            // Recurrence logic: Generate next instance BEFORE marking complete
            if ($task->is_recurring && $task->recurrence_pattern) {
                // generateNextRecurrence handles the limit check and updates the old task status
                $this->generateNextRecurrence($task, 'completed');
            } else {
                // Non-recurring task
                $task->markComplete();
            }
            $this->saveTasks();
            return true;
        }
        return false;
    }
    
    // V19.0: Skip Task Implementation
    public function markTaskSkipped(int $id): bool {
        $task = $this->getTask($id);
        if ($task) {
            if (!$task->is_recurring || !$task->recurrence_pattern) {
                echo "\033[31m[ERROR] Task #{$id} ('{$task->title}') is not a recurring task and cannot be skipped.\033[0m\n";
                return false;
            }

            // Generate next instance, marking the current one as archived
            // We use 'archived' as the status to set for the old instance.
            $result = $this->generateNextRecurrence($task, 'archived');
            
            // Save the state regardless of whether a new task was generated (limits might have been reached)
            $this->saveTasks();
            return true;
        }
        return false;
    }
    
    public function archiveTask(int $id): bool {
        $task = $this->getTask($id);
        if ($task) {
            $task->markArchived();
            $this->saveTasks();
            return true;
        }
        return false;
    }

    public function deleteTask(int $id): bool {
        $initialCount = count($this->tasks);
        $this->tasks = array_filter($this->tasks, fn(Task $task) => $task->id != $id);
        $this->tasks = array_values($this->tasks); // Re-index
        if (count($this->tasks) < $initialCount) {
            $this->saveTasks();
            return true;
        }
        return false;
    }

    public function updateTask(int $id, ?string $title = null, ?string $description = null, ?string $priority = null, ?string $dueDate = null, ?bool $isRecurring = null, ?string $recurrencePattern = null, ?int $limitCount = null, ?string $recurrenceEndDate = null, ?array $tags = null, ?array $dependencies = null): bool { // V16.0: Added dependencies
        $task = $this->getTask($id);
        if ($task) {
            if ($title !== null) $task->title = $title;
            if ($description !== null) $task->description = $description;
            
            // Strict Date/Time Validation applied here (returns UTC DateTime)
            if ($dueDate !== null) {
                $task->due_date = $this->validateDateTime($dueDate);
            }
            
            if ($priority !== null) {
                $task->priority = $task->normalizePriority($priority);
            }
            
            if ($isRecurring !== null) {
                $task->is_recurring = $isRecurring;
            }
            
            if ($recurrencePattern !== null) {
                $normalizedPattern = $task->normalizeRecurrence($recurrencePattern);
                if ($normalizedPattern === null && $recurrencePattern !== '') {
                     throw new Exception("Invalid recurrence pattern: '{$recurrencePattern}'. Use keywords or ISO 8601 duration (P#D, P#W, P#M).");
                }
                $task->recurrence_pattern = $normalizedPattern;
            }
            
            // Handle recurrence limits
            if ($limitCount !== null) {
                if ($limitCount <= 0) {
                    throw new Exception("Recurrence limit count must be a positive integer.");
                }
                $task->recurrence_limit_count = $limitCount;
            }
            
            // Validate recurrence end date (returns UTC DateTime)
            if ($recurrenceEndDate !== null) {
                $task->recurrence_end_date = $this->validateDateTime($recurrenceEndDate);
            }
            
            // V15.2: Handle tags
            if ($tags !== null) {
                $task->tags = $task->normalizeTags($tags);
            }
            
            // V16.0: Handle dependencies
            if ($dependencies !== null) {
                $normalizedDependencies = $task->normalizeDependencies($dependencies);
                
                // V16.0: Check for circular dependency
                if (!empty($normalizedDependencies)) {
                    try {
                        $this->detectCircularDependency($id, $normalizedDependencies);
                    } catch (Exception $e) {
                         throw new Exception("Cannot update dependencies for task #{$id}: " . $e->getMessage());
                    }
                }
                
                $task->dependencies = $normalizedDependencies;
            }
            
            // Handle clearing limits if recurrence is turned off
            if ($isRecurring === false) {
                $task->recurrence_limit_count = null;
                $task->recurrence_end_date = null;
            }
            
            $task->updateStatus(); 
            $this->saveTasks();
            return true;
        }
        return false;
    }
    
    // --- Maintenance (V14.0) ---
    
    /**
     * Permanently deletes completed or archived tasks older than $days.
     */
    public function pruneTasks(int $days, bool $completedOnly = false): int {
        if ($days <= 0) {
            throw new Exception("Days must be a positive integer for pruning.");
        }
        
        $initialCount = count($this->tasks);
        
        // Calculate the cutoff date/time (V15.1: uses UTC for cutoff calculation)
        $utc = new DateTimeZone('UTC');
        $cutoff = new DateTime("-{$days} days", $utc);
        
        $this->tasks = array_filter($this->tasks, function(Task $task) use ($cutoff, $completedOnly) {
            // Check if the task is older than the cutoff date based on creation time (both are UTC)
            $is_old = $task->created_at < $cutoff;
            
            if ($task->status === 'completed' && $is_old) {
                // Prune completed tasks older than cutoff
                return false; 
            }
            
            if (!$completedOnly && $task->status === 'archived' && $is_old) {
                // Prune archived tasks older than cutoff (if not completedOnly mode)
                return false;
            }
            
            // Keep all other tasks 
            return true;
        });
        
        $this->tasks = array_values($this->tasks); // Re-index
        $deletedCount = $initialCount - count($this->tasks);
        
        if ($deletedCount > 0) {
            $this->saveTasks();
        }
        
        return $deletedCount;
    }


    // --- View Command Implementation ---
    
    public function viewTask(int $id): string {
        $task = $this->getTask($id);
        if (!$task) {
            return "\n\033[31mERROR: Task #{$id} not found.\033[0m\n";
        }

        // V15.1: Display Timezone in header
        $output = "\n\033[1;33m--- Task Details: #{$task->id} ({$this->userTimezone->getName()}) ---\033[0m\n";
        $output .= str_repeat('-', 80) . "\n";
        
        $status_color = $this->getColorForStatus($task->status);
        $priority_color = $this->getColorForPriority($task->priority);
        $reset_color = "\033[0m";
        
        $recurring_info = $task->is_recurring ? 
            "Yes (" . ($task->recurrence_pattern ?? 'Unknown') . ")" : 
            "No";

        $limit_info = '';
        if ($task->recurrence_limit_count !== null) {
            $limit_info .= "Count: {$task->recurrence_limit_count} remaining.";
        }
        
        // V15.1: Convert recurrence end date to user TZ for display
        $displayRecurrenceEndDate = $task->recurrence_end_date instanceof DateTime ? $this->convertToUserTimezone($task->recurrence_end_date) : null;
        
        if ($displayRecurrenceEndDate instanceof DateTime) {
            if ($limit_info) $limit_info .= " / ";
            $limit_info .= "Until: {$displayRecurrenceEndDate->format('Y-m-d')}";
        }
        if (!$limit_info && $task->is_recurring) {
            $limit_info = "None (Infinite)";
        }

        // V15.1: Convert UTC dates to USER_TIMEZONE for display
        $displayDueDate = $task->due_date instanceof DateTime ? $this->convertToUserTimezone($task->due_date) : null;
        $displayCreatedAt = $task->created_at instanceof DateTime ? $this->convertToUserTimezone($task->created_at) : null;

        $dueDateStr = $displayDueDate instanceof DateTime ? $displayDueDate->format('Y-m-d H:i:s') : 'N/A';
        $createdAtStr = $displayCreatedAt instanceof DateTime ? $displayCreatedAt->format('Y-m-d H:i:s') : 'N/A';
        
        // V15.2: Tags display
        $tags_info = empty($task->tags) ? 'None' : implode(', ', $task->tags);
        
        // V16.0: Dependencies display
        $dependencies_info = empty($task->dependencies) ? 'None' : implode(', ', array_map(fn($id) => "#$id", $task->dependencies));
        
        // V20.0: Dependent tasks display
        $dependentTasks = $this->getDependentTasks($id);
        $dependentInfo = empty($dependentTasks) ? 'None' : implode(', ', array_map(fn(Task $t) => "#{$t->id} ({$t->title})", $dependentTasks));


        $output .= sprintf("%-15s: %s\n", "Title", $task->title);
        $output .= sprintf("%-15s: {$status_color}%s{$reset_color}\n", "Status", ucfirst($task->status));
        $output .= sprintf("%-15s: {$priority_color}%s{$reset_color}\n", "Priority", $task->priority);
        $output .= sprintf("%-15s: %s\n", "Due Date", $dueDateStr);
        $output .= sprintf("%-15s: %s\n", "Created At", $createdAtStr);
        $output .= sprintf("%-15s: %s\n", "Tags", $tags_info); // V15.2
        $output .= sprintf("%-15s: %s\n", "Dependencies", $dependencies_info); // V16.0
        $output .= sprintf("%-15s: %s\n", "Dependents", $dependentInfo); // V20.0
        $output .= sprintf("%-15s: %s\n", "Recurring", $recurring_info);
        
        if ($task->is_recurring) { 
            $output .= sprintf("%-15s: %s\n", "Rec. Limit", $limit_info);
        }
        
        $output .= str_repeat('-', 80) . "\n";
        $output .= "\033[1;37mDescription:\033[0m\n";
        $output .= wordwrap($task->description, 78) . "\n";
        $output .= str_repeat('-', 80) . "\n";

        return $output;
    }

    // --- Listing and Formatting ---

    public function listTasks(string $filter = 'all', string $sortBy = 'urgency'): string {
        $filter = strtolower($filter);
        $filteredTasks = [];
        
        // V16.0: Enhanced Tag Parsing Setup
        $requiredTags = []; // Tags prefixed with + (AND)
        $excludedTags = []; // Tags prefixed with - (NOT)
        $optionalTags = []; // Tags without prefix (OR, only used if no required/excluded tags exist)
        $filterDisplay = $filter;

        // V16.0: Check if filter is a tag filter (e.g., "tag:+work,-bug,home")
        if (strpos($filter, 'tag:') === 0) {
            $tagString = substr($filter, 4);
            $rawTags = array_map('trim', explode(',', $tagString));
            
            foreach ($rawTags as $rawTag) {
                if (empty($rawTag)) continue;
                
                if (strpos($rawTag, '+') === 0) {
                    $requiredTags[] = strtolower(substr($rawTag, 1));
                } elseif (strpos($rawTag, '-') === 0) {
                    $excludedTags[] = strtolower(substr($rawTag, 1));
                } else {
                    $optionalTags[] = strtolower($rawTag);
                }
            }
            
            $filter = 'tag_filter'; // Internal marker
            $filterDisplay = "Tags: " . $tagString;
        }
        
        $allowed_statuses = ['pending', 'completed', 'overdue', 'archived'];

        // 1. Update statuses and filter
        foreach ($this->tasks as $task) {
            $task->updateStatus(); // Uses UTC comparison
            
            $match = false;
            
            if ($filter === 'tag_filter') { // V16.0 Enhanced Tag Filtering
                if ($task->status === 'archived') continue; 
                
                $taskTags = $task->tags;
                $isMatch = true;

                // 1. Check Excluded Tags (NOT)
                foreach ($excludedTags as $eTag) {
                    if (in_array($eTag, $taskTags)) {
                        $isMatch = false;
                        break;
                    }
                }
                if (!$isMatch) continue; 

                // 2. Check Required Tags (AND)
                foreach ($requiredTags as $rTag) {
                    if (!in_array($rTag, $taskTags)) {
                        $isMatch = false;
                        break;
                    }
                }
                if (!$isMatch) continue;

                // 3. Check Default/Optional Tags (OR) - Only if no explicit constraints were used.
                if (empty($requiredTags) && empty($excludedTags) && !empty($optionalTags)) {
                    $hasOptionalMatch = false;
                    foreach ($optionalTags as $oTag) {
                        if (in_array($oTag, $taskTags)) {
                            $hasOptionalMatch = true;
                            break;
                        }
                    }
                    if (!$hasOptionalMatch) {
                        $isMatch = false;
                    }
                }
                
                // If we passed all checks, and we had *some* constraints, we match.
                if ($isMatch && (!empty($requiredTags) || !empty($excludedTags) || !empty($optionalTags))) {
                    $match = true;
                }
                
            } elseif ($filter === 'all') {
                // Default 'all' filter excludes archived tasks from the main list
                if ($task->status !== 'archived') {
                    $match = true;
                }
            } elseif ($filter === 'archived' && $task->status === 'archived') {
                $match = true;
            } elseif (in_array($filter, $allowed_statuses) && $task->status === $filter) {
                $match = true;
            } elseif (in_array($filter, ['high', 'medium', 'low']) && strtolower($task->priority) === $filter) {
                $match = true;
            }

            if ($match) {
                $filteredTasks[] = $task;
            }
        }

        // 2. Sorting (uses UTC timestamps for consistency)
        usort($filteredTasks, function(Task $a, Task $b) use ($sortBy) {
            if ($sortBy === 'urgency') {
                // Urgency order: Overdue(1) > Pending High(2) > Pending Medium(3) > Pending Low(4) > Completed(5) > Archived(6)
                $statusOrder = ['overdue' => 1, 'pending' => 2, 'completed' => 5, 'archived' => 6];
                $priorityOrder = ['High' => 0, 'Medium' => 1, 'Low' => 2]; 

                $a_val = $statusOrder[$a->status] ?? 7;
                $b_val = $statusOrder[$b->status] ?? 7;

                // If both are pending, sort by priority
                if ($a->status === 'pending' && $b->status === 'pending') {
                    $a_prio = $priorityOrder[$a->priority] ?? 3;
                    $b_prio = $priorityOrder[$b->priority] ?? 3;
                    return $a_prio <=> $b_prio;
                }

                // Primary sort by status group
                $statusComparison = $a_val <=> $b_val;
                if ($statusComparison !== 0) {
                    return $statusComparison;
                }

                // Tertiary sort by due date (for tasks within the same status group) - uses UTC timestamps
                $timeA = $a->due_date instanceof DateTime ? $a->due_date->getTimestamp() : PHP_INT_MAX;
                $timeB = $b->due_date instanceof DateTime ? $b->due_date->getTimestamp() : PHP_INT_MAX;
                return $timeA <=> $timeB;

            } elseif ($sortBy === 'priority') {
                $priorityOrder = ['High' => 3, 'Medium' => 2, 'Low' => 1];
                $a_prio = $priorityOrder[$a->priority] ?? 0;
                $b_prio = $priorityOrder[$b->priority] ?? 0;
                return $b_prio <=> $a_prio; 
            } elseif ($sortBy === 'due_date') {
                // Uses UTC timestamps
                $timeA = $a->due_date instanceof DateTime ? $a->due_date->getTimestamp() : PHP_INT_MAX;
                $timeB = $b->due_date instanceof DateTime ? $b->due_date->getTimestamp() : PHP_INT_MAX;
                return $timeA <=> $timeB;
            } elseif ($sortBy === 'created_at') { 
                // Uses UTC timestamps
                $timeA = $a->created_at instanceof DateTime ? $a->created_at->getTimestamp() : PHP_INT_MAX;
                $timeB = $b->created_at instanceof DateTime ? $b->created_at->getTimestamp() : PHP_INT_MAX;
                return $timeA <=> $timeB;
            }
            // Default sort by ID
            return $a->id <=> $b->id;
        });

        // 3. Format output
        return $this->formatTasks($filteredTasks, $filterDisplay, $sortBy);
    }

    public function formatTasks(array $tasks, string $filter, string $sortBy): string {
        // V15.1: Display Timezone in header
        $output = "\n--- Task List (Filter: " . ucfirst($filter) . ", Sort: " . ucfirst($sortBy) . ") [TZ: {$this->userTimezone->getName()}] ---\n";
        
        if (empty($tasks)) {
            $output .= "\033[36m[INFO] No tasks found matching the criteria.\033[0m\n";
            $output .= str_repeat('-', 80) . "\n";
            return $output;
        }
        
        $output .= str_repeat('-', 80) . "\n";
        // V16.0: Adjusted Title width (16->14) and added Dep column (3)
        $output .= sprintf("| %-4s | %-14s | %-10s | %-8s | %-15s | %-5s | %-8s | %-3s |\n", "ID", "Title", "Status", "Prio", "Due Date/Time", "Rec", "Tags", "Dep"); 
        $output .= str_repeat('-', 80) . "\n";
        
        foreach ($tasks as $task) {
            $reset_color = "\033[0m";
            
            $status_color = $this->getColorForStatus($task->status);
            $priority_color = $this->getColorForPriority($task->priority);

            // V15.1: Convert UTC date to USER_TIMEZONE for display (truncated for list view)
            $displayDueDate = $task->due_date instanceof DateTime ? $this->convertToUserTimezone($task->due_date) : null;
            $dueDateStr = $displayDueDate instanceof DateTime ? $displayDueDate->format('Y-m-d H:i') : 'N/A';
            
            // Display recurrence pattern + limit indicator
            $recurrenceStr = $task->is_recurring ? substr($task->recurrence_pattern, 0, 5) : 'No';
            if ($task->is_recurring) {
                if ($task->recurrence_limit_count !== null) {
                    $recurrenceStr = $task->recurrence_limit_count . "x";
                } elseif ($task->recurrence_end_date instanceof DateTime) {
                    $recurrenceStr = "End";
                }
            }
            
            // V15.2: Tags display (truncated)
            $tagsStr = empty($task->tags) ? 'None' : substr(implode(',', $task->tags), 0, 8);
            
            // V16.0: Dependency indicator
            $depStr = !empty($task->dependencies) ? 'Yes' : 'No';
            
            $output .= sprintf(
                "| %-4d | %-14s | {$status_color}%-10s{$reset_color} | {$priority_color}%-8s{$reset_color} | %-15s | %-5s | %-8s | %-3s |\n",
                $task->id,
                substr($task->title, 0, 14), // Title truncated to 14
                ucfirst($task->status),
                $task->priority,
                $dueDateStr,
                $recurrenceStr,
                $tagsStr, // V15.2
                $depStr // V16.0
            );
        }
        $output .= str_repeat('-', 80) . "\n";
        return $output;
    }
    
    /**
     * Searches tasks by keyword in title or description.
     */
    public function searchTasks(string $keyword, bool $includeArchived = false): string { 
        $keyword = strtolower($keyword);
        $filteredTasks = [];

        foreach ($this->tasks as $task) {
            $task->updateStatus(); 

            if (stripos($task->title, $keyword) !== false || stripos($task->description, $keyword) !== false) {
                if ($includeArchived || $task->status !== 'archived') {
                    $filteredTasks[] = $task;
                }
            }
        }

        $filter_str = "Search: '{$keyword}'" . ($includeArchived ? " (Incl. Archived)" : "");
        return $this->formatTasks($filteredTasks, $filter_str, 'urgency');
    }

    // --- Export Command Implementation ---
    public function exportTasks(string $filter = 'all'): string {
        $filter = strtolower($filter);
        $filteredTasks = [];
        
        foreach ($this->tasks as $task) {
            $task->updateStatus();
            
            $match = false;
            // ... filtering logic ...
            if ($filter === 'all') {
                $match = true;
            } elseif (in_array($filter, ['pending', 'completed', 'overdue', 'archived']) && $task->status === $filter) {
                $match = true;
            } elseif (in_array($filter, ['high', 'medium', 'low']) && strtolower($task->priority) === $filter) {
                $match = true;
            }

            if ($match) {
                $filteredTasks[] = $task;
            }
        }
        
        if (empty($filteredTasks)) {
            return "\033[36m[INFO] No tasks found matching the criteria for export.\033[0m\n";
        }
        
        $csv_data = fopen('php://temp', 'r+');
        
        // V16.0: Updated header for dependencies
        $header = ['ID', 'Title', 'Description', 'Due Date (UTC)', 'Priority', 'Status', 'Created At (UTC)', 'Is Recurring', 'Recurrence Pattern', 'Recurrence Limit Count', 'Recurrence End Date (UTC)', 'Tags', 'Dependencies']; 
        fputcsv($csv_data, $header);
        
        // Write task data
        foreach ($filteredTasks as $task) {
            // Since $task->due_date is UTC, we use its format directly for export
            $dueDateStr = $task->due_date instanceof DateTime ? $task->due_date->format('Y-m-d H:i:s') : '';
            $createdAtStr = $task->created_at instanceof DateTime ? $task->created_at->format('Y-m-d H:i:s') : '';
            // Recurrence End Date (Date only format)
            $endDateStr = $task->recurrence_end_date instanceof DateTime ? $task->recurrence_end_date->format('Y-m-d') : ''; 
            
            // V15.2: Tags (using pipe | as separator for CSV field)
            $tagsStr = implode('|', $task->tags);
            
            // V16.0: Dependencies (using comma as separator for CSV field)
            $dependenciesStr = implode(',', $task->dependencies);
            
            $row = [
                $task->id,
                $task->title,
                $task->description,
                $dueDateStr,
                $task->priority,
                $task->status,
                $createdAtStr,
                $task->is_recurring ? 'Yes' : 'No',
                $task->recurrence_pattern,
                $task->recurrence_limit_count,
                $endDateStr,
                $tagsStr, // V15.2
                $dependenciesStr // V16.0
            ];
            fputcsv($csv_data, $row);
        }
        
        rewind($csv_data);
        $csv_string = stream_get_contents($csv_data);
        fclose($csv_data);

        $export_filename = 'tasks_export_' . date('Ymd_His') . '.csv';
        if (@file_put_contents($export_filename, $csv_string) === false) {
             $error_message = error_get_last()['message'] ?? 'Unknown file write error.';
             return "\033[31mCRITICAL ERROR: Failed to export tasks to {$export_filename}. Error: {$error_message}\033[0m\n";
        }
        
        return "\033[32mSUCCESS: Exported " . count($filteredTasks) . " tasks (in UTC) to {$export_filename}\033[0m\n";
    }

    // --- CLI Argument Parsing (V16.0: Added dependencies) ---
    
    private function parseFlexibleArguments(array $args, bool $requireTitleAndDesc = false): array {
        $updates = [];
        $i = 0;
        
        // V16.0: Added 'dependencies'
        $allowedFields = ['title', 'description', 'priority', 'due_date', 'is_recurring', 'recurrence_pattern', 'include_archived', 'limit', 'until', 'completed_only', 'tags', 'dependencies']; 

        while ($i < count($args)) {
            $arg = $args[$i];
            if (strpos($arg, '--') === 0) {
                $field = substr($arg, 2);
                $value = $args[$i + 1] ?? null;
                
                // Special handling for boolean flags
                if ($field === 'include-archived') {
                    $updates['include_archived'] = true;
                    $i += 1;
                    continue;
                }
                
                // V14.0: Special handling for boolean flag
                if ($field === 'completed-only') {
                    $updates['completed_only'] = true;
                    $i += 1;
                    continue;
                }

                if ($value === null) {
                    throw new Exception("Missing value for field '{$field}'. Usage: --field value.");
                }
                
                // Map common aliases
                if ($field === 'prio') $field = 'priority';
                if ($field === 'due') $field = 'due_date';
                if ($field === 'desc') $field = 'description';
                if ($field === 'rec') $field = 'is_recurring';
                if ($field === 'pattern') $field = 'recurrence_pattern';
                if ($field === 'tag') $field = 'tags'; // V15.2 Alias
                if ($field === 'depends') $field = 'dependencies'; // V16.0 Alias
                
                // V11.0: Map limit/until
                if ($field === 'limit') $field = 'recurrence_limit_count';
                if ($field === 'until') $field = 'recurrence_end_date';
                
                if (in_array($field, $allowedFields)) {
                    
                    $processedValue = $value;
                    
                    // Special handling for boolean/null fields
                    if ($field === 'is_recurring') {
                        $processedValue = filter_var($value, FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE);
                        if ($processedValue === null) {
                             throw new Exception("Invalid value for --rec. Use 'true' or 'false'.");
                        }
                    }
                    
                    // V11.0: Handle limit count (must be positive integer)
                    elseif ($field === 'recurrence_limit_count') {
                        if (!is_numeric($value) || intval($value) != $value || $value < 1) {
                            throw new Exception("Invalid value for --limit. Must be a positive integer.");
                        }
                        $processedValue = (int)$value;
                    }
                    
                    // V15.2: Handle tags (comma-separated string to array)
                    elseif ($field === 'tags') {
                        $processedValue = array_map('trim', explode(',', $value));
                    }
                    
                    // V16.0: Handle dependencies (comma-separated string to array of integers)
                    elseif ($field === 'dependencies') {
                        $raw_ids = array_map('trim', explode(',', $value));
                        $processedValue = array_filter($raw_ids, 'is_numeric');
                        $processedValue = array_map('intval', $processedValue);
                    }
                    
                    $updates[$field] = $processedValue;
                    $i += 2; // Move past field and value
                } else {
                    throw new Exception("Unknown field '{$field}' provided.");
                }
            } else {
                $i++;
            }
        }
        
        if ($requireTitleAndDesc) {
            if (!isset($updates['title'])) {
                throw new Exception("Missing required field: --title.");
            }
            if (!isset($updates['description'])) {
                throw new Exception("Missing required field: --desc.");
            }
        }
        
        return $updates;
    }


    // --- CLI Handler ---

    public function handleCli(array $argv) {
        if (count($argv) < 2) {
            $this->showHelp();
            return;
        }

        $command = strtolower($argv[1]);

        try {
            switch ($command) {
                case 'self_test':
                    $this->self_test();
                    break;
                case 'add':
                    if (count($argv) < 4) {
                        throw new Exception("Usage: add --title \"T\" --desc \"D\" [...]");
                    }
                    
                    $args = $this->parseFlexibleArguments(array_slice($argv, 2), true);
                    
                    $task = $this->addTask(
                        $args['title'],
                        $args['description'],
                        $args['priority'] ?? 'Medium',
                        $args['due_date'] ?? null,
                        $args['is_recurring'] ?? false,
                        $args['recurrence_pattern'] ?? null,
                        $args['recurrence_limit_count'] ?? null,
                        $args['recurrence_end_date'] ?? null,
                        $args['tags'] ?? [], // V15.2
                        $args['dependencies'] ?? [] // V16.0
                    );
                    echo "\033[32mSUCCESS: Task #{$task->id} added.\033[0m\n";
                    break;
                case 'list':
                    $filter = $argv[2] ?? 'all';
                    $sortBy = $argv[3] ?? 'urgency';
                    echo $this->listTasks($filter, $sortBy);
                    break;
                case 'search': 
                    if (count($argv) < 3) {
                        throw new Exception("Usage: search <keyword> [--include-archived]");
                    }
                    
                    $keyword = $argv[2];
                    $args = $this->parseFlexibleArguments(array_slice($argv, 3), false);
                    $includeArchived = $args['include_archived'] ?? false;

                    echo $this->searchTasks($keyword, $includeArchived);
                    break;
                case 'view': 
                    if (count($argv) < 3 || !is_numeric($argv[2])) {
                        throw new Exception("Usage: view <task_id>");
                    }
                    $id = (int)$argv[2];
                    echo $this->viewTask($id);
                    break;
                case 'complete':
                    if (count($argv) < 3 || !is_numeric($argv[2])) {
                        throw new Exception("Usage: complete <task_id>");
                    }
                    $id = (int)$argv[2];
                    if ($this->markTaskComplete($id)) {
                        echo "\033[32mSUCCESS: Task #{$id} marked as completed.\033[0m\n";
                    } else {
                        // Error message handled inside markTaskComplete
                    }
                    break;
                case 'skip': // V19.0: Skip recurring task
                    if (count($argv) < 3 || !is_numeric($argv[2])) {
                        throw new Exception("Usage: skip <task_id>");
                    }
                    $id = (int)$argv[2];
                    if ($this->markTaskSkipped($id)) {
                        echo "\033[32mSUCCESS: Task #{$id} skipped. Next instance generated.\033[0m\n";
                    } else {
                        // Error message handled inside markTaskSkipped
                    }
                    break;
                case 'archive':
                    if (count($argv) < 3 || !is_numeric($argv[2])) {
                        throw new Exception("Usage: archive <task_id>");
                    }
                    $id = (int)$argv[2];
                    if ($this->archiveTask($id)) {
                        echo "\033[32mSUCCESS: Task #{$id} marked as archived.\033[0m\n";
                    } else {
                        echo "\033[31mERROR: Task #{$id} not found or already archived.\033[0m\n";
                    }
                    break;
                case 'delete':
                    if (count($argv) < 3 || !is_numeric($argv[2])) {
                        throw new Exception("Usage: delete <task_id>");
                    }
                    $id = (int)$argv[2];
                    if ($this->deleteTask($id)) {
                        echo "\033[32mSUCCESS: Task #{$id} deleted.\003[0m\n";
                    } else {
                        echo "\033[31mERROR: Task #{$id} not found for deletion.\003[0m\n";
                    }
                    break;
                case 'update': 
                    if (count($argv) < 4 || !is_numeric($argv[2])) {
                        throw new Exception("Usage: update <id> --field value. Fields: --title, --desc, --prio, --due, --rec, --pattern, --limit, --until, --tags, --depends.");
                    }
                    $id = (int)$argv[2];
                    
                    $updates = $this->parseFlexibleArguments(array_slice($argv, 3), false);
                    
                    if (empty($updates)) {
                        throw new Exception("No fields provided for update. Usage: update <id> --field value.");
                    }

                    $title = $updates['title'] ?? null;
                    $description = $updates['description'] ?? null;
                    $priority = $updates['priority'] ?? null;
                    $dueDate = $updates['due_date'] ?? null; 
                    $isRecurring = $updates['is_recurring'] ?? null;
                    $recurrencePattern = $updates['recurrence_pattern'] ?? null;
                    $limitCount = $updates['recurrence_limit_count'] ?? null; 
                    $recurrenceEndDate = $updates['recurrence_end_date'] ?? null; 
                    $tags = $updates['tags'] ?? null; // V15.2
                    $dependencies = $updates['dependencies'] ?? null; // V16.0


                    if ($this->updateTask($id, $title, $description, $priority, $dueDate, $isRecurring, $recurrencePattern, $limitCount, $recurrenceEndDate, $tags, $dependencies)) { // V16.0
                        echo "\033[32mSUCCESS: Task #{$id} updated.\033[0m\n";
                    } else {
                        echo "\033[31mERROR: Task #{$id} not found for update.\033[0m\n";
                    }
                    break;
                case 'prune': 
                    if (count($argv) < 3 || !is_numeric($argv[2])) {
                        throw new Exception("Usage: prune <days> [--completed-only]");
                    }
                    $days = (int)$argv[2];
                    
                    $args = $this->parseFlexibleArguments(array_slice($argv, 3), false);
                    $completedOnly = $args['completed_only'] ?? false;
                    
                    if ($days <= 0) {
                        throw new Exception("Prune days must be a positive integer.");
                    }

                    $deleted = $this->pruneTasks($days, $completedOnly);
                    echo "\033[32mSUCCESS: Pruned {$deleted} tasks older than {$days} days.\033[0m\n";
                    break;
                case 'export':
                    $filter = $argv[2] ?? 'all';
                    echo $this->exportTasks($filter);
                    break;
                case 'help':
                    $this->showHelp();
                    break;
                default:
                    echo "\033[31mUnknown command: {$command}\033[0m\n";
                    $this->showHelp();
                    break;
            }
        } catch (Exception $e) {
            echo "\033[31mCLI Error: " . $e->getMessage() . "\033[0m\n";
        }
    }

    private function showHelp() {
        echo "\n===================================================================\n";
        echo "SUPER HYBRID PHP Task Manager CLI (V20.0 - Reverse Dependency)\n";
        echo "===================================================================\n";
        echo "Usage: php solution.php <command> [arguments]\n\n";
        echo "Timezone: " . USER_TIMEZONE . " (All input dates are interpreted in this TZ)\n\n";
        echo "Commands:\n";
        echo "  self_test                               Run internal tests.\n";
        echo "  add --title T --desc D [...]             Add a new task (Requires --title and --desc).\n";
        echo "    Optional fields: --prio, --due (YYYY-MM-DD [HH:MM:SS] or relative like 'tomorrow 9am', '+1 week'), --rec, --pattern, --limit (#), --until (YYYY-MM-DD or relative), --tags (tag1,tag2), --depends (1,2,3).\n"; 
        echo "  list [filter] [sortBy]                   List tasks.\n";
        echo "    Filters: all (default, excludes archived), pending, completed, overdue, archived, high, medium, low, tag:<+req,-exc,opt,...>.\n"; 
        echo "      Tag Syntax: Precede tags with '+' for AND (required) or '-' for NOT (excluded). Unprefixed tags use OR logic (only if no +/- tags are present).\n";
        echo "    Sorts: urgency (default), id, priority, due_date, created_at.\n";
        echo "  search <keyword> [--include-archived]    Search tasks by title or description.\n"; 
        echo "  view <id>                                Show full details of a task (includes dependent tasks).\n"; // Updated help
        echo "  update <id> --field value                Update task details (Flexible Args).\n";
        echo "    Fields: --title, --desc, --prio, --due, --rec, --pattern, --limit, --until, --tags, --depends.\n"; 
        echo "  complete <id>                            Mark a task as completed (generates next recurrence, checks dependencies).\n";
        echo "  skip <id>                                Skip the current recurring task instance (archives it and generates the next).\n"; // V19.0
        echo "  archive <id>                             Move a task to the archive.\n";
        echo "  delete <id>                              Permanently delete a task.\n";
        echo "  prune <days> [--completed-only]          Permanently delete old completed/archived tasks.\n"; 
        echo "  export [filter]                          Export tasks to CSV (Dates are UTC).\n";
        echo "  help                                     Show this help message.\n";
        echo "===================================================================\n";
    }
    
    // --- Self Test Helper (V15.1) ---
    private function setTaskCreationDate(int $id, string $dateString) {
        $task = $this->getTask($id);
        if ($task) {
            try {
                // Ensure test date string is parsed in UTC context for consistency with storage
                $utc = new DateTimeZone('UTC');
                $task->created_at = new DateTime($dateString, $utc);
                return true;
            } catch (Exception $e) {
                echo "\033[31m[ERROR] Failed to set creation date for task #{$id}: {$e->getMessage()}\033[0m\n";
                return false;
            }
        }
        return false;
    }

    // --- Self Test (V20.0: Reverse Dependency) ---
    public function self_test() {
        echo "\n\033[1;35m--- Super Hybrid Task Manager Self Test Initiated (V20.0) ---\033[0m\n";
        
        $testFile = 'test_hybrid_tasks_v20_0.json';
        
        // Cleanup previous test data
        if (file_exists($testFile)) {
            unlink($testFile);
        }
        
        // Initialize manager (triggers loadTasks and processRecurrenceQueue)
        $manager = new TaskManager($testFile);
        echo "\033[36m[INFO] Storage reset.[0m\n";
        
        // --- Setup Dates ---
        $today = date('Y-m-d');
        $yesterday_date = date('Y-m-d', strtotime('-1 day')); 
        $five_minutes_ago = date('Y-m-d H:i:s', strtotime('-5 minutes'));
        $five_minutes_from_now = date('Y-m-d H:i:s', strtotime('+5 minutes'));

        // 1. Testing Recurrence and Time Precision (T1-T4)
        echo "\n\033[1;37m1. Testing Recurrence and Time Precision:\033[0m\n";
        
        // T1: Overdue Daily Recurring Task (P1D) - Should trigger recurrence on load
        $t1 = $manager->addTask("Daily Report", "Send daily status report.", "High", $yesterday_date, true, 'daily'); 
        
        // T2: Time-based overdue task (Should be overdue immediately)
        $t2 = $manager->addTask("Overdue Time Check", "Check if time status works.", "High", $five_minutes_ago); 
        
        // T3: Time-based pending task (Should be pending)
        $t3 = $manager->addTask("Pending Time Check", "Check if time status works.", "Medium", $five_minutes_from_now); 
        
        // Check: Verify time-based status 
        if ($t2->status === 'overdue' && $t3->status === 'pending') {
            echo "\033[32m[PASS] T2/T3: Time-based status check successful. T2: {$t2->status}, T3: {$t3->status}.[0m\n";
        } else {
            echo "\033[31m[FAIL] T2/T3: Time-based status check failed. T2: {$t2->status}, T3: {$t3->status}.[0m\n";
        }
        
        // Check T1 recurrence generation (T4 generated)
        $manager_reload = new TaskManager($testFile);
        $t1_reloaded = $manager_reload->getTask($t1->id);
        $t4_id = $manager_reload->generateUniqueId() - 1; 
        $t4 = $manager_reload->getTask($t4_id); 
        
        $t4_due_date_dt_user = $t4 ? $manager_reload->convertToUserTimezone($t4->due_date) : null;
        $t4_due_date_str_date = $t4_due_date_dt_user ? $t4_due_date_dt_user->format('Y-m-d') : 'N/A';

        if ($t1_reloaded && $t1_reloaded->status === 'archived' && $t4 && $t4_due_date_str_date === $today) {
            echo "\033[32m[PASS] T1 successfully archived and T4 generated (P1D).[0m\n";
        } else {
            echo "\033[31m[FAIL] Overdue Recurrence failed. T1 Status: " . ($t1_reloaded->status ?? 'N/A') . ". T4 Due: " . $t4_due_date_str_date . " (Expected {$today})[0m\n";
        }
        
        // 2. Testing Dependency Tracking (T5-T9)
        echo "\n\033[1;37m2. Testing Dependency Tracking:\033[0m\n";
        
        // T5: The dependency task (Parent)
        $t5 = $manager_reload->addTask("Dependency T5 (Parent)", "Must be completed first.", "Low");
        $t5_id = $t5->id;
        
        // T6: Dependent task (Child 1 - depends on T5)
        $t6 = $manager_reload->addTask("Dependent T6 (Child 1)", "Waits for T5.", "High", null, false, null, null, null, [], [$t5_id]);
        $t6_id = $t6->id;
        
        // T8: Dependent task (Child 2 - depends on T5)
        $t8 = $manager_reload->addTask("Dependent T8 (Child 2)", "Also waits for T5.", "Medium", null, false, null, null, null, [], [$t5_id]);
        $t8_id = $t8->id;
        
        // 2a. Attempt to complete T6 (should fail)
        ob_start();
        $success_fail_1 = $manager_reload->markTaskComplete($t6_id);
        $fail_output = ob_get_clean();
        $t6_status = $manager_reload->getTask($t6_id)->status;
        
        if (strpos($fail_output, 'Cannot complete Task') !== false && $t6_status === 'pending' && $success_fail_1 === false) {
            echo "\033[32m[PASS] 2a: Completion of T6 successfully blocked by uncompleted dependency T5.[0m\n";
        } else {
            echo "\033[31m[FAIL] 2a: Completion of T6 should have been blocked. Status: {$t6_status}. Output: {$fail_output}[0m\n";
        }
        
        // 2b. Test Circular Dependency Prevention (T7 -> T9 -> T7)
        $t7 = $manager_reload->addTask("T7 (A)", "Simple", "Low");
        $t7_id = $t7->id;
        $t9 = $manager_reload->addTask("T9 (B)", "Depends on T7", "Low", null, false, null, null, null, [], [$t7_id]);
        $t9_id = $t9->id;
        
        // Try to update T7 to depend on T9, creating a cycle.
        $cli_args_cycle = ['solution.php', 'update', $t7_id, '--depends', $t9_id];
        ob_start();
        $manager_reload->handleCli($cli_args_cycle);
        $cycle_output = ob_get_clean();
        
        if (strpos($cycle_output, 'Circular dependency detected') !== false) {
            echo "\033[32m[PASS] 2b: Circular dependency update (T7 depends on T9) successfully blocked.[0m\n";
        } else {
            echo "\033[31m[FAIL] 2b: Circular dependency update failed to block. Output: {$cycle_output}[0m\n";
        }
        
        // 2c. Test Reverse Dependency Reporting (V20.0)
        // T5 should list T6 and T8 as dependents.
        $cli_args_view_t5 = ['solution.php', 'view', $t5_id];
        ob_start();
        $manager_reload->handleCli($cli_args_view_t5);
        $output_view_t5 = ob_get_clean();
        
        // Expected string format: Dependents: #ID (Title), #ID (Title)
        $expected_dependent_str_1 = "#{$t6_id} (Dependent T6 (Child 1))";
        $expected_dependent_str_2 = "#{$t8_id} (Dependent T8 (Child 2))";
        
        $found_dep_1 = strpos($output_view_t5, $expected_dependent_str_1) !== false;
        $found_dep_2 = strpos($output_view_t5, $expected_dependent_str_2) !== false;
        
        
        if ($found_dep_1 && $found_dep_2) {
            echo "\033[32m[PASS] 2c: Reverse Dependency Reporting (V20.0) successful. T5 lists T6 and T8 as dependents.[0m\n";
        } else {
            echo "\033[31m[FAIL] 2c: Reverse Dependency Reporting failed. Found T6: " . ($found_dep_1 ? 'Yes' : 'No') . ", Found T8: " . ($found_dep_2 ? 'Yes' : 'No') . ". Output:\n{$output_view_t5}\n[0m\n";
        }

        // 3. Testing Enhanced Tag Filtering (T10-T12)
        echo "\n\033[1;37m3. Testing Enhanced Tag Filtering:\033[0m\n";
        
        // T10: Tags: [review, deploy, high]
        $t10 = $manager_reload->addTask("Task with Tags A", "Needs review and deployment.", "High", $today, false, null, null, null, ['review', 'deploy', 'high']);
        $t10_id = $t10->id;
        
        // T11: Tags: [review, docs, low]
        $t11 = $manager_reload->addTask("Documentation Task B", "Write guide.", "Low", $today, false, null, null, null, ['review', 'docs', 'low']);
        $t11_id = $t11->id;
        
        // T12: Tags: [bug, deploy, high]
        $t12 = $manager_reload->addTask("Bug Fix Task C", "Urgent bug fix.", "High", $today, false, null, null, null, ['bug', 'deploy', 'high']);
        $t12_id = $t12->id;
        
        // 3a. Test AND logic: Find tasks with +deploy AND +high (Should match T10 and T12)
        $cli_args_filter_and = ['solution.php', 'list', 'tag:+deploy,+high'];
        ob_start();
        $manager_reload->handleCli($cli_args_filter_and);
        $output_and = ob_get_clean();
        
        // FIX: Search for '| ID ' to avoid false positives from date strings (e.g., '10' in '2025-10-07')
        $found_t10_and = strpos($output_and, '| ' . (string)$t10_id . ' ') !== false;
        $found_t11_and = strpos($output_and, '| ' . (string)$t11_id . ' ') !== false;
        $found_t12_and = strpos( $output_and, '| ' . (string)$t12_id . ' ') !== false;
        
        if ($found_t10_and && !$found_t11_and && $found_t12_and) {
            echo "\033[32m[PASS] 3a: AND filtering '+deploy,+high' correctly matched T10 and T12.[0m\n";
        } else {
            echo "\033[31m[FAIL] 3a: AND filtering failed. Found T10: " . ($found_t10_and ? 'Yes' : 'No') . ", T11: " . ($found_t11_and ? 'Yes' : 'No') . ", T12: " . ($found_t12_and ? 'Yes' : 'No') . " (Expected: Yes, No, Yes)[0m\n";
        }
        
        // 3b. Test Combined logic: Find tasks +deploy,-bug (Should match T10 only)
        $cli_args_filter_combined = ['solution.php', 'list', 'tag:+deploy,-bug'];
        ob_start();
        $manager_reload->handleCli($cli_args_filter_combined);
        $output_combined = ob_get_clean();
        
        // FIX: Search for '| ID ' to avoid false positives from date strings
        $found_t10_combined = strpos($output_combined, '| ' . (string)$t10_id . ' ') !== false;
        $found_t11_combined = strpos($output_combined, '| ' . (string)$t11_id . ' ') !== false;
        $found_t12_combined = strpos($output_combined, '| ' . (string)$t12_id . ' ') !== false;
        
        if ($found_t10_combined && !$found_t11_combined && !$found_t12_combined) {
            echo "\033[32m[PASS] 3b: Combined filtering '+deploy,-bug' correctly matched T10 only.[0m\n";
        } else {
            echo "\033[31m[FAIL] 3b: Combined filtering failed. Found T10: " . ($found_t10_combined ? 'Yes' : 'No') . ", T11: " . ($found_t11_combined ? 'Yes' : 'No') . ", T12: " . ($found_t12_combined ? 'Yes' : 'No') . " (Expected: Yes, No, No)[0m\n";
        }
        
        // 4. Testing Recurrence Skip (V19.0)
        echo "\n\033[1;37m4. Testing Recurrence Skip:\033[0m\n";
        
        // T13: Weekly Recurring Task (P1W) due yesterday
        $t13_due = date('Y-m-d', strtotime('-1 day')); 
        $t13 = $manager_reload->addTask("Weekly Skip Test", "Test skipping a weekly task.", "Medium", $t13_due, true, 'weekly'); 
        $t13_id = $t13->id;
        
        // 4a. Perform the skip operation
        $manager_reload->markTaskSkipped($t13_id);
        
        // T13 should now be archived. The next generated task (T14) should be due 7 days after the original due date.
        $t14_id = $manager_reload->generateUniqueId() - 1; 
        $t14 = $manager_reload->getTask($t14_id);
        $t13_reloaded = $manager_reload->getTask($t13_id);
        
        // Calculate expected due date for T14 (7 days after T13's original due date)
        $expected_t14_due = date('Y-m-d', strtotime('+1 week', strtotime($t13_due)));
        
        $t14_due_dt_user = $t14 ? $manager_reload->convertToUserTimezone($t14->due_date) : null;
        $t14_due_date_str_date = $t14_due_dt_user ? $t14_due_dt_user->format('Y-m-d') : 'N/A';
        
        if ($t13_reloaded && $t13_reloaded->status === 'archived' && $t14 && $t14_due_date_str_date === $expected_t14_due) {
            echo "\033[32m[PASS] 4a: Skip operation successful. T13 archived, T14 generated with next due date ({$expected_t14_due}).[0m\n";
        } else {
            echo "\033[31m[FAIL] 4a: Skip operation failed. T13 Status: " . ($t13_reloaded->status ?? 'N/A') . ". T14 Due: " . $t14_due_date_str_date . " (Expected {$expected_t14_due})[0m\n";
        }
        
        // 4b. Test skipping a non-recurring task (should fail gracefully)
        $t15 = $manager_reload->addTask("Non-Recur Skip Test", "Cannot be skipped.", "Low");
        $t15_id = $t15->id;
        
        ob_start();
        $success_fail_2 = $manager_reload->markTaskSkipped($t15_id);
        $fail_output_2 = ob_get_clean();
        
        if (strpos($fail_output_2, 'is not a recurring task and cannot be skipped') !== false && $success_fail_2 === false) {
            echo "\033[32m[PASS] 4b: Skipping non-recurring task T15 successfully blocked.[0m\n";
        } else {
            echo "\033[31m[FAIL] 4b: Skipping non-recurring task T15 failed to block. Output: {$fail_output_2}[0m\n";
        }

        // 5. Final Cleanup
        if (file_exists($testFile)) {
            unlink($testFile);
        }

        echo "\n\033[1;35m--- Super Hybrid Task Manager Self Test Finished (V20.0) ---[0m\n";
    }
}

// --- Execution Entry Point ---
if (php_sapi_name() == 'cli') {
    // Ensure $argv is available in CLI context
    if (!isset($argv)) {
        $argv = $_SERVER['argv'] ?? [];
    }
    
    // Note: We use the default tasks.json for operational use
    $manager = new TaskManager(); 
    
    // If the only argument is the script name, run self_test automatically for verification
    if (count($argv) == 1) {
        $manager->self_test();
    } else {
        $manager->handleCli($argv);
    }
}
?>
