<?php
// created by https://github.com/joaopauloschuler/beyond-python-smolagents
/**
 * Task Manager Class - Ultimate Hybrid v8.0 (Advanced Analytics)
 * Features: Colors, Archive, Completion Tracking, Hard Delete/Purge, Dedicated Tag Management, Time-to-Completion Metric, Tag Exclusion Filtering, Aggregate Completion Analytics (Avg & Median Time, Priority Distribution).
 */
class TaskManager {
    private array $tasks = []; // Indexed by ID
    private string $storageFile = 'tasks.json';
    private int $nextId = 1;

    // Constants (From S1)
    const STATUS_PENDING = 'Pending';
    const STATUS_IN_PROGRESS = 'In Progress';
    const STATUS_COMPLETED = 'Completed';
    const STATUS_ARCHIVED = 'Archived'; 
    const VALID_STATUSES = [self::STATUS_PENDING, self::STATUS_IN_PROGRESS, self::STATUS_COMPLETED, self::STATUS_ARCHIVED]; 

    const PRIORITY_LOW = 'Low';
    const PRIORITY_MEDIUM = 'Medium';
    const PRIORITY_HIGH = 'High';
    const VALID_PRIORITIES = [self::PRIORITY_LOW, self::PRIORITY_MEDIUM, self::PRIORITY_HIGH];
    const PRIORITY_MAP = [self::PRIORITY_HIGH => 3, self::PRIORITY_MEDIUM => 2, self::PRIORITY_LOW => 1];

    // ANSI Color Codes for CLI Output (From S1)
    const COLOR_RESET = "\033[0m";
    const COLOR_RED = "\033[31m";
    const COLOR_GREEN = "\033[32m";
    const COLOR_YELLOW = "\033[33m";
    const COLOR_MAGENTA = "\033[35m"; 
    const COLOR_BOLD = "\033[1m";

    public function __construct() {
        $this->loadTasks();
    }

    private function generateId(): int {
        return $this->nextId++;
    }
    
    /**
     * Loads tasks and the next available ID from the storage file. (Robust loading)
     */
    private function loadTasks(): void {
        if (file_exists($this->storageFile)) {
            $data = file_get_contents($this->storageFile);
            
            if ($data === false) { 
                 return;
            }
            
            $decoded = json_decode($data, true);
            
            if (json_last_error() !== JSON_ERROR_NONE || !is_array($decoded)) {
                return; 
            }

            if (isset($decoded['tasks'])) {
                
                $maxId = 0;
                $this->tasks = [];
                
                $taskList = $decoded['tasks'] ?? [];
                
                foreach ($taskList as $item) {
                    // Reconstruct Task object from array data
                    try { 
                        $task = Task::fromArray($item);
                        $this->tasks[$task->id] = $task; // Store tasks by ID
                        
                        if ($task->id > $maxId) {
                            $maxId = $task->id;
                        }
                    } catch (\Throwable $e) {
                        error_log("Skipping corrupted task entry during load: " . $e->getMessage());
                        continue;
                    }
                }
                
                $persistedNextId = $decoded['nextId'] ?? 0;

                // Robustness: Calculate nextId based on max ID found or persisted value
                if ($persistedNextId > $maxId) {
                    $this->nextId = $persistedNextId;
                } else {
                    $this->nextId = $maxId + 1;
                }
            }
        }
    }

    /**
     * Saves the current tasks and next ID to the storage file.
     */
    private function saveTasks(): void {
        $serializedTasks = array_map(function(Task $t): array { return $t->toArray(); }, $this->tasks);
        
        $data = [
            'tasks' => array_values($serializedTasks), 
            'nextId' => $this->nextId
        ];
        
        file_put_contents($this->storageFile, json_encode($data, JSON_PRETTY_PRINT));
    }
    
    /**
     * Adds a new task with input sanitization and strict validation.
     * @throws \InvalidArgumentException If priority is invalid.
     */
    public function addTask(string $description, string $priority = self::PRIORITY_MEDIUM, ?string $dueDate = null, array $tags = []): int {
        $id = $this->generateId();
        
        // Input Sanitization
        $description = trim(strip_tags($description));
        
        // --- Refined Validation: Priority ---
        $normalizedPriority = null;
        foreach (self::VALID_PRIORITIES as $canonicalPriority) {
            if (strtolower($priority) === strtolower($canonicalPriority)) {
                $normalizedPriority = $canonicalPriority;
                break;
            }
        }
        
        if ($normalizedPriority === null) {
            throw new \InvalidArgumentException("Invalid priority provided: {$priority}. Must be one of: " . implode(', ', self::VALID_PRIORITIES));
        }

        // --- Tag Normalization: Enforce lowercase ---
        $normalizedTags = array_unique(array_map('trim', array_map('strtolower', $tags)));

        $task = new Task($id, $description, $normalizedPriority, $dueDate, self::STATUS_PENDING, date('Y-m-d H:i:s'), null, $normalizedTags);
        $this->tasks[$id] = $task;
        $this->saveTasks();
        return $id;
    }

    /**
     * Updates an existing task with input sanitization and strict validation, including completedAt tracking.
     * 
     * IMPROVEMENT: Throws InvalidArgumentException for invalid status/priority.
     * @throws \InvalidArgumentException
     */
    public function updateTask(int $id, array $updates): bool {
        if (!isset($this->tasks[$id])) {
            return false;
        }

        $task = $this->tasks[$id];

        if (isset($updates['description'])) {
            // Input Sanitization
            $task->description = trim(strip_tags($updates['description']));
        }
        
        if (isset($updates['status'])) {
            $inputStatus = $updates['status'];
            $canonicalStatus = null;
            
            foreach (self::VALID_STATUSES as $status) {
                if (strtolower($inputStatus) === strtolower($status)) {
                    $canonicalStatus = $status;
                    break;
                }
            }
            
            if ($canonicalStatus === null) { 
                // IMPROVEMENT: Throw exception for invalid status
                throw new \InvalidArgumentException("Invalid status provided for update: {$inputStatus}. Must be one of: " . implode(', ', self::VALID_STATUSES));
            }
            
            // HYBRID LOGIC: Update completedAt timestamp
            if ($canonicalStatus === self::STATUS_COMPLETED) {
                if (!$task->completedAt) {
                    $task->completedAt = new \DateTimeImmutable();
                }
            } elseif ($task->completedAt && $canonicalStatus !== self::STATUS_COMPLETED) {
                $task->completedAt = null;
            }
            
            $task->status = $canonicalStatus;
        }
        
        if (isset($updates['priority'])) {
            $inputPriority = $updates['priority'];
            $canonicalPriority = null;
            
            foreach (self::VALID_PRIORITIES as $priority) {
                if (strtolower($inputPriority) === strtolower($priority)) {
                    $canonicalPriority = $priority;
                    break;
                }
            }
            
            if ($canonicalPriority === null) { 
                // IMPROVEMENT: Throw exception for invalid priority
                throw new \InvalidArgumentException("Invalid priority provided for update: {$inputPriority}. Must be one of: " . implode(', ', self::VALID_PRIORITIES));
            }
            $task->priority = $canonicalPriority;
        }
        
        if (isset($updates['dueDate'])) {
            // Task class handles conversion from string to DateTimeImmutable
            $task->setDueDate($updates['dueDate']);
        }
        
        if (isset($updates['tags']) && is_array($updates['tags'])) {
            // Enforce lowercase for tags
            $updates['tags'] = array_map('trim', $updates['tags']);
            $updates['tags'] = array_map('strtolower', $updates['tags']); 
            $task->tags = array_unique($updates['tags']);
        }

        $this->saveTasks();
        return true;
    }
    
    /**
     * Adds a single tag to a task.
     */
    public function addTag(int $id, string $tag): bool {
        if (!isset($this->tasks[$id])) {
            return false;
        }
        $tag = trim(strip_tags($tag));
        if (empty($tag)) return false;
        
        // Enforce lowercase
        $tag = strtolower($tag);
        
        $task = $this->tasks[$id];
        if (!in_array($tag, $task->tags, true)) {
            $task->tags[] = $tag;
            $this->saveTasks();
            return true;
        }
        return false;
    }
    
    /**
     * Removes a single tag from a task.
     */
    public function removeTag(int $id, string $tag): bool {
        if (!isset($this->tasks[$id])) {
            return false;
        }
        
        $task = $this->tasks[$id];
        $initialCount = count($task->tags);
        
        // Comparison is case-insensitive
        $tagLower = strtolower($tag);
        $task->tags = array_filter($task->tags, function(string $t) use ($tagLower): bool {
            return $t !== $tagLower; // Since tags are stored lowercase, direct comparison is fine after normalizing input
        });
        
        // Re-index array
        $task->tags = array_values($task->tags);

        if (count($task->tags) < $initialCount) {
            $this->saveTasks();
            return true;
        }
        return false;
    }
    
    /**
     * Dedicated method to quickly change a task's status with strict validation, including completedAt tracking.
     * @throws \InvalidArgumentException
     */
    public function markTaskStatus(int $id, string $newStatus): bool {
        if (!isset($this->tasks[$id])) {
            return false;
        }
        
        $canonicalStatus = null;
        foreach (self::VALID_STATUSES as $status) {
            if (strtolower($newStatus) === strtolower($status)) {
                $canonicalStatus = $status;
                break;
            }
        }
        
        if ($canonicalStatus) {
            $task = $this->tasks[$id];
            
            // HYBRID LOGIC: Update completedAt timestamp
            if ($canonicalStatus === self::STATUS_COMPLETED) {
                if (!$task->completedAt) {
                    // Use a slight delay for testing time-to-completion metric
                    $task->completedAt = (new \DateTimeImmutable())->modify('+1 second'); 
                }
            } elseif ($task->completedAt && $canonicalStatus !== self::STATUS_COMPLETED) {
                $task->completedAt = null;
            }
            
            $task->status = $canonicalStatus;
            $this->saveTasks();
            return true;
        }
        
        // Throw exception for invalid status
        throw new \InvalidArgumentException("Invalid status provided: {$newStatus}. Must be one of: " . implode(', ', self::VALID_STATUSES));
    }
    
    /**
     * Deletes a task (performs soft deletion by setting status to Archived).
     */
    public function deleteTask(int $id): bool {
        if (isset($this->tasks[$id])) {
            $this->tasks[$id]->status = self::STATUS_ARCHIVED;
            $this->saveTasks();
            return true;
        }
        return false;
    }
    
    /**
     * Permanently deletes a task. (NEW FEATURE)
     */
    public function hardDeleteTask(int $id): bool {
        if (isset($this->tasks[$id])) {
            unset($this->tasks[$id]);
            $this->saveTasks();
            return true;
        }
        return false;
    }

    /**
     * Permanently removes all tasks marked as Archived. (NEW FEATURE)
     * @return int The number of tasks purged.
     */
    public function purgeArchivedTasks(): int {
        $initialCount = count($this->tasks);
        $this->tasks = array_filter($this->tasks, function(Task $task): bool {
            return $task->status !== self::STATUS_ARCHIVED;
        });
        $purgedCount = $initialCount - count($this->tasks);
        
        if ($purgedCount > 0) {
            $this->saveTasks();
        }
        return $purgedCount;
    }

    /**
     * Retrieves all unique tags used across all tasks, sorted alphabetically.
     */
    public function getUniqueTags(): array {
        $tags = [];
        foreach ($this->tasks as $task) {
            $tags = array_merge($tags, $task->tags);
        }
        $tags = array_unique($tags);
        sort($tags);
        return $tags;
    }
    
    /**
     * Calculates the time taken to complete a task. (NEW FEATURE)
     * @return string|null Formatted time difference or null if not completed.
     */
    public function getTimeToCompletion(int $id): ?string {
        if (!isset($this->tasks[$id]) || $this->tasks[$id]->status !== self::STATUS_COMPLETED) {
            return null;
        }
        
        $task = $this->tasks[$id];
        
        if (!$task->createdAt || !$task->completedAt) {
            return null;
        }
        
        try {
            // Ensure completedAt is later than createdAt for diff calculation
            if ($task->completedAt < $task->createdAt) {
                return "Error: Completed time precedes creation time.";
            }

            $interval = $task->createdAt->diff($task->completedAt);
            
            // Format the interval nicely (Years, Months, Days, Hours, Minutes, Seconds)
            $parts = [];
            if ($interval->y > 0) $parts[] = $interval->y . ' years';
            if ($interval->m > 0) $parts[] = $interval->m . ' months';
            if ($interval->d > 0) $parts[] = $interval->d . ' days';
            if ($interval->h > 0) $parts[] = $interval->h . ' hours';
            if ($interval->i > 0) $parts[] = $interval->i . ' minutes';
            if ($interval->s > 0) $parts[] = $interval->s . ' seconds';

            return empty($parts) ? 'Less than a second' : implode(', ', array_slice($parts, 0, 3)); // Show up to 3 largest units
            
        } catch (\Throwable $e) {
            return null;
        }
    }
    
    /**
     * Helper function to format seconds into a human-readable duration (Y, M, D, H, M). (From S2)
     */
    private function formatDuration(int $seconds): string {
        if ($seconds < 0) {
            return "N/A";
        }
        // Use DateTime objects to accurately calculate duration components
        $dtF = new \DateTime('@0');
        $dtT = new \DateTime("@$seconds");
        $diff = $dtF->diff($dtT);

        $parts = [];
        if ($diff->y > 0) $parts[] = $diff->y . 'y';
        if ($diff->m > 0) $parts[] = $diff->m . 'mo';
        if ($diff->d > 0) $parts[] = $diff->d . 'd';
        if ($diff->h > 0) $parts[] = $diff->h . 'h';
        if ($diff->i > 0) $parts[] = $diff->i . 'm';
        
        // If duration is less than 1 minute, show seconds
        if (empty($parts) && $seconds > 0) {
            return $seconds . 's';
        }
        
        return empty($parts) ? '0s' : implode(' ', $parts);
    }

    /**
     * Calculates the median of an array of numbers. (NEW v8.0)
     * @param array<int> $durations Array of completion times in seconds.
     * @return int The median duration in seconds, or 0 if empty.
     */
    private static function calculateMedian(array $durations): int {
        if (empty($durations)) {
            return 0;
        }
        sort($durations);
        $count = count($durations);
        $middleIndex = (int) floor($count / 2);

        if ($count % 2 === 1) {
            // Odd number of elements
            return $durations[$middleIndex];
        } else {
            // Even number of elements
            // Median is the average of the two middle elements
            $low = $durations[$middleIndex - 1];
            $high = $durations[$middleIndex];
            return (int) round(($low + $high) / 2);
        }
    }


    /**
     * Views tasks based on advanced filters and sorting. 
     * 
     * @param bool $showCompleted If false, hides tasks with STATUS_COMPLETED and STATUS_ARCHIVED.
     * @return Task[]
     */
    public function viewTasks(array $filters = [], string $sortBy = 'id', string $sortOrder = 'asc', bool $showCompleted = true): array {
        $filteredTasks = array_values($this->tasks); 
        
        // 0. Default Filter: Hide completed and archived tasks unless explicitly requested
        if (!$showCompleted) {
            $filteredTasks = array_filter($filteredTasks, function(Task $task): bool {
                return $task->status !== self::STATUS_COMPLETED && $task->status !== self::STATUS_ARCHIVED;
            });
        }

        // 1. Filtering
        foreach ($filters as $key => $value) {
            // Skip if value is empty or null, unless key is a defined filter
            if (empty($value) && !is_array($value) && !in_array($key, ['status', 'priority'])) continue;

            $filteredTasks = array_filter($filteredTasks, function(Task $task) use ($key, $value): bool {
                
                // 1. Tags filtering (Must include tag)
                if ($key === 'tags' && is_string($value)) {
                    $requiredTag = strtolower($value); 
                    return in_array($requiredTag, $task->tags, true); 
                }
                
                // 2. Tag Exclusion Filtering (Must NOT include tag) (NEW FEATURE)
                if ($key === 'tag_exclude' && is_string($value)) {
                    $excludedTags = array_map('strtolower', array_map('trim', explode(',', $value))); 
                    
                    // Check if the task has any of the excluded tags
                    foreach ($task->tags as $taskTag) {
                        if (in_array($taskTag, $excludedTags, true)) {
                            return false; // Exclude this task
                        }
                    }
                    return true; // Keep this task
                }
                
                // 3. Partial Description Search
                if ($key === 'description_like' && is_string($value)) {
                    return stripos($task->description, $value) !== false;
                }

                // 4. Multiple Status Filtering
                if ($key === 'status' && is_array($value)) {
                    $normalizedStatuses = array_map('strtolower', $value);
                    return in_array(strtolower($task->status), $normalizedStatuses, true);
                }
                
                // 5. Date Range Filtering (Due Before)
                if ($key === 'due_before' && is_string($value)) {
                    try {
                        if (!$task->dueDate) return true; 
                        $limitDate = (new \DateTimeImmutable($value))->setTime(23, 59, 59);
                        return $task->dueDate <= $limitDate;
                    } catch (\Throwable $e) { 
                        error_log("Invalid date format for due_before filter: {$value}");
                        return false; 
                    } 
                }
                
                // 6. Date Range Filtering (Due After)
                if ($key === 'due_after' && is_string($value)) {
                    try {
                        if (!$task->dueDate) return true;
                        $limitDate = (new \DateTimeImmutable($value))->setTime(0, 0, 0);
                        return $task->dueDate >= $limitDate;
                    } catch (\Throwable $e) { 
                        error_log("Invalid date format for due_after filter: {$value}");
                        return false; 
                    }
                }

                // 7. Standard field filtering (Single Status or Priority)
                if (in_array($key, ['status', 'priority'], true)) {
                    $taskValue = $task->$key ?? null;
                    return strtolower((string)$taskValue) === strtolower((string)$value);
                }
                
                // 8. Fallback for ID comparison
                $taskValue = $task->$key ?? null;
                return $taskValue == $value;
            });
        }

        // 2. Sorting
        if (!empty($filteredTasks)) {
            usort($filteredTasks, function(Task $a, Task $b) use ($sortBy, $sortOrder): int {
                
                // Handle sorting by date fields
                if (in_array($sortBy, ['dueDate', 'createdAt', 'completedAt'], true)) {
                    // Use a very large number for null dates so they sort last (asc) or first (desc)
                    $valA = $a->$sortBy ? $a->$sortBy->getTimestamp() : PHP_INT_MAX; 
                    $valB = $b->$sortBy ? $b->$sortBy->getTimestamp() : PHP_INT_MAX;
                    
                    // If both are null, they are equal
                    if ($valA === PHP_INT_MAX && $valB === PHP_INT_MAX) return 0;
                    
                } else {
                    $valA = $a->$sortBy ?? null;
                    $valB = $b->$sortBy ?? null;
                }

                // Custom sorting for Priority: High > Medium > Low
                if ($sortBy === 'priority') {
                    $valA = self::PRIORITY_MAP[$valA] ?? 0;
                    $valB = self::PRIORITY_MAP[$valB] ?? 0;
                }
                
                // Null handling for non-date fields
                if ($valA === null && $valB !== null) return ($sortOrder === 'desc') ? 1 : -1;
                if ($valA !== null && $valB === null) return ($sortOrder === 'desc') ? -1 : 1;
                if ($valA === null && $valB === null) return 0;

                $comparison = 0;
                if ($valA < $valB) {
                    $comparison = -1;
                } elseif ($valA > $valB) {
                    $comparison = 1;
                }

                return ($sortOrder === 'desc') ? -$comparison : $comparison;
            });
        }

        return $filteredTasks;
    }

    /**
     * Generates a summary report of the tasks, including completion analytics (v8.0).
     */
    public function getSummary(): string {
        $allTasks = array_values($this->tasks);
        $total = count($allTasks);
        
        $pending = 0;
        $inProgress = 0;
        $completed = 0;
        $archived = 0;
        $high_prio = 0;
        $overdue = 0;
        
        // Analytics variables (v8.0)
        $totalCompletionSeconds = 0;
        $completedCountForAnalytics = 0; 
        $completionDurations = []; // Collect durations for median
        $priorityCounts = array_fill_keys(self::VALID_PRIORITIES, 0); // Initialize priority counts

        foreach ($allTasks as $task) {
            if ($task->status === self::STATUS_PENDING) {
                $pending++;
            } elseif ($task->status === self::STATUS_IN_PROGRESS) {
                $inProgress++;
            } elseif ($task->status === self::STATUS_COMPLETED) {
                $completed++;
                
                // Calculate completion time for analytics
                if ($task->createdAt && $task->completedAt) {
                    $duration = $task->completedAt->getTimestamp() - $task->createdAt->getTimestamp();
                    if ($duration >= 0) {
                        $totalCompletionSeconds += $duration;
                        $completedCountForAnalytics++;
                        $completionDurations[] = $duration; // Collect duration
                    }
                }
                
            } elseif ($task->status === self::STATUS_ARCHIVED) {
                $archived++;
            }
            
            // Count priorities (for all tasks)
            $priorityCounts[$task->priority]++;
            
            if ($task->priority === self::PRIORITY_HIGH) {
                $high_prio++;
            }

            if ($task->isOverdue()) {
                $overdue++;
            }
        }
        
        // Calculate Analytics (v8.0)
        $completionRate = $total > 0 ? round(($completed / $total) * 100, 2) : 0.00;
        
        $averageCompletionTime = 0;
        $averageCompletionTimeFormatted = "N/A";
        
        if ($completedCountForAnalytics > 0) {
            $averageCompletionTime = (int)($totalCompletionSeconds / $completedCountForAnalytics);
            $averageCompletionTimeFormatted = $this->formatDuration($averageCompletionTime);
        }

        // Median Completion Time (NEW v8.0)
        $medianCompletionTime = self::calculateMedian($completionDurations);
        $medianCompletionTimeFormatted = $this->formatDuration($medianCompletionTime);

        // Priority Distribution (NEW v8.0)
        $prioritySummary = "Priority Distribution:\n";
        foreach (self::VALID_PRIORITIES as $prio) {
            $count = $priorityCounts[$prio] ?? 0;
            $percentage = $total > 0 ? round(($count / $total) * 100, 1) : 0.0;
            // Use ANSI color for priority names
            $color = match ($prio) {
                self::PRIORITY_HIGH => self::COLOR_RED,
                self::PRIORITY_MEDIUM => self::COLOR_YELLOW,
                default => self::COLOR_RESET,
            };
            $prioritySummary .= sprintf("  - %s%-6s%s: %4d tasks (%5.1f%%)\n", $color, $prio, self::COLOR_RESET, $count, $percentage);
        }


        $summary = "\n--- Task Manager Summary (v8.0 Advanced Analytics) ---\n";
        $summary .= "Total Tasks: $total\n";
        $summary .= "Pending Tasks: $pending\n";
        $summary .= "In Progress Tasks: $inProgress\n";
        $summary .= "Completed Tasks: $completed\n";
        $summary .= "Archived Tasks: $archived\n";
        $summary .= "High Priority Tasks: $high_prio\n";
        $summary .= "Overdue Pending/In Progress Tasks: $overdue\n";
        $summary .= "--------------------------------------------\n";
        $summary .= sprintf("Completion Rate: %.2f%%\n", $completionRate);
        $summary .= "Avg. Completion Time: {$averageCompletionTimeFormatted}\n";
        $summary .= "Med. Completion Time: {$medianCompletionTimeFormatted}\n"; // NEW
        $summary .= "--------------------------------------------\n";
        $summary .= $prioritySummary; // NEW
        $summary .= "--------------------------------------------\n";
        return $summary;
    }


    /**
     * Helper function to determine row color based on task status, priority, and overdue status.
     */
    private function getTaskColor(Task $task): string {
        if ($task->status === self::STATUS_COMPLETED) {
            return self::COLOR_GREEN;
        }
        if ($task->status === self::STATUS_ARCHIVED) {
            return self::COLOR_MAGENTA;
        }
        if ($task->isOverdue()) {
            return self::COLOR_BOLD . self::COLOR_RED; // Bold Red for Overdue
        }
        if ($task->priority === self::PRIORITY_HIGH) {
            return self::COLOR_RED;
        }
        if ($task->status === self::STATUS_IN_PROGRESS) {
            return self::COLOR_YELLOW;
        }
        return self::COLOR_RESET;
    }


    /**
     * Formats tasks for CLI display (v6.0: Added Time to Completion).
     * @param Task[] $tasks
     * @return string
     */
    public function formatTasksForDisplay(array $tasks): string {
        $output = "\n";
        if (empty($tasks)) {
            return $output . "--- No tasks found ---\n";
        }

        // 1. Calculate dynamic widths
        $maxDescWidth = 30;
        $maxTagsWidth = 20;
        $limit = 50; 

        foreach ($tasks as $task) {
            $descLength = strlen($task->description);
            $tagsLength = strlen(implode(', ', $task->tags));
            
            $maxDescWidth = min($limit, max($maxDescWidth, $descLength));
            $maxTagsWidth = min($limit, max($maxTagsWidth, $tagsLength));
        }
        // Ensure minimum width for headers
        $maxDescWidth = max($maxDescWidth, strlen("Description"));
        $maxTagsWidth = max($maxTagsWidth, strlen("Tags"));


        // 2. Define column widths and format string
        // Fixed widths: ID(4), Status(12), Priority(8), Due Date(10), Created At(19), Completed At(19)
        $col_widths = [4, $maxDescWidth, 12, 8, 10, 19, 19, $maxTagsWidth]; 
        
        $line = "+";
        foreach ($col_widths as $width) {
            $line .= str_repeat('-', $width + 2) . "+";
        }
        $line .= "\n";
        
        // Format string for data rows
        $formatString = "| %-4s | %-{$maxDescWidth}s | %-12s | %-8s | %-10s | %-19s | %-19s | %-{$maxTagsWidth}s |";
        
        $output .= $line;
        $output .= sprintf($formatString . "\n", "ID", "Description", "Status", "Priority", "Due Date", "Created At", "Completed At", "Tags"); 
        $output .= $line;

        foreach ($tasks as $task) {
            $color = $this->getTaskColor($task);
            
            // Truncate only if necessary
            $desc = substr($task->description, 0, $maxDescWidth);
            $tags = substr(implode(', ', $task->tags), 0, $maxTagsWidth);
            
            $dueDate = $task->dueDate ? $task->dueDate->format('Y-m-d') : 'N/A';
            $createdAt = $task->createdAt ? $task->createdAt->format('Y-m-d H:i:s') : 'N/A';
            $completedAt = $task->completedAt ? $task->completedAt->format('Y-m-d H:i:s') : 'N/A'; 
            
            $output .= $color; // Apply color to the whole row
            $output .= sprintf($formatString . "\n",
                $task->id,
                $desc,
                $task->status,
                $task->priority,
                $dueDate,
                substr($createdAt, 0, 19),
                substr($completedAt, 0, 19), 
                $tags
            );
            $output .= self::COLOR_RESET; // Reset color after the row
        }
        $output .= $line;
        return $output;
    }
    
    /**
     * Retrieves and formats the detailed information for a single task. (v6.0: Added Time to Completion)
     */
    public function getTaskDetails(int $id): string {
        if (!isset($this->tasks[$id])) {
            return "Error: Task ID $id not found.\n";
        }

        $task = $this->tasks[$id];
        
        $output = "\n--- Task Details (ID: {$task->id}) ---\n";
        $output .= sprintf("Description: %s\n", $task->description);
        $output .= sprintf("Status:      %s\n", $task->status);
        $output .= sprintf("Priority:    %s\n", $task->priority);
        $output .= sprintf("Created At:  %s\n", $task->createdAt->format('Y-m-d H:i:s'));
        
        $dueDate = $task->dueDate ? $task->dueDate->format('Y-m-d') : 'N/A';
        $output .= sprintf("Due Date:    %s\n", $dueDate);
        
        $completedAt = $task->completedAt ? $task->completedAt->format('Y-m-d H:i:s') : 'N/A'; 
        $output .= sprintf("Completed At: %s\n", $completedAt);
        
        // NEW: Time to Completion Metric
        $timeToCompletion = $this->getTimeToCompletion($id);
        if ($timeToCompletion) {
             $output .= sprintf("Time to Complete: %s\n", $timeToCompletion);
        }
        
        if ($task->dueDate && $task->isOverdue()) {
             $output .= self::COLOR_BOLD . self::COLOR_RED . "OVERDUE:     YES! (As of today)" . self::COLOR_RESET . "\n";
        }

        $tags = empty($task->tags) ? 'None' : implode(', ', $task->tags);
        $output .= sprintf("Tags:        %s\n", $tags);
        $output .= "---------------------------------\n";

        return $output;
    }

    /**
     * Displays CLI help information. (v8.0)
     */
    public function showHelp(): string {
        $help = "\n--- Task Manager CLI Help (v8.0 Ultimate Hybrid: Advanced Analytics, Hard Delete, Tag Mgmt, Time Tracking) ---\n";
        $help .= "Usage: php solution2.php <command> [arguments]\n\n";
        $help .= "Commands:\n";
        $help .= "  add <description> [--priority=P] [--due=YYYY-MM-DD] [--tags=T1,T2]\n";
        $help .= "    Example: php solution2.php add 'Fix bug 123' --priority=High --tags=code,urgent\n";
        $help .= "    NOTE: Invalid priorities will cause an error due to strict validation.\n\n";
        $help .= "  view [--status=S] [--tag=T] [--tag_exclude=T] [--sort=F] [--order=O] [--completed] [--due_before=D] [--due_after=D]\n";
        $help .= "    Filters: --status=Pending,InProgress, --tag=urgent, --tag_exclude=personal, --sort=completedAt, --order=desc\n";
        $help .= "    Note: By default, completed AND archived tasks are hidden. Use --completed to show all.\n\n";
        $help .= "  show <id>\n";
        $help .= "    Displays full details of a single task, including Time to Completion.\n\n";
        $help .= "  update <id> [--desc=D] [--status=S] [--priority=P] [--due=D] [--tags=T1,T2]\n";
        $help .= "    Example: php solution2.php update 5 --status='In Progress' --due=2024-12-31\n";
        $help .= "    NOTE: Invalid status/priority now throws an exception for clearer feedback.\n\n";
        $help .= "  status <id> <new_status>\n";
        $help .= "    Example: php solution2.php status 5 Completed\n\n";
        $help .= "  tag-add <id> <tag> (Adds a single tag)\n";
        $help .= "  tag-remove <id> <tag> (Removes a single tag)\n\n";
        $help .= "  delete <id> (Performs soft delete by setting status to Archived)\n"; 
        $help .= "  hard-delete <id> (PERMANENTLY removes the task)\n"; 
        $help .= "  purge (PERMANENTLY removes all Archived tasks)\n"; 
        $help .= "  summary (Includes Completion Rate, Avg. Completion Time, Med. Completion Time, and Priority Distribution)\n";
        $help .= "  tags (Lists all unique tags used across active and archived tasks)\n"; 
        $help .= "  test (Runs internal self_test)\n";
        $help .= "------------------------------------\n";
        return $help;
    }


    /**
     * Self-test method with static inputs for testing all features, including v8.0 additions.
     */
    public function self_test(): void {
        echo "==================================================\n";
        echo "--- Task Manager Ultimate Hybrid Self Test Started (v8.0) ---\n";
        echo "==================================================\n";

        // 1. Initialize and ensure clean state
        echo "\n[1] Initializing Task Manager (clearing previous data and tasks.json).\n";
        $storageFile = $this->storageFile;
        if (file_exists($storageFile)) {
             unlink($storageFile);
             // Re-initialize internal state after file removal
             $this->tasks = [];
             $this->nextId = 1;
        }

        // 2. Add tasks 
        echo "\n[2] Adding initial tasks:\n";
        $overdueDate = date('Y-m-d', strtotime('-1 day'));
        $tomorrowDate = date('Y-m-d', strtotime('+1 day'));
        $farFutureDate = date('Y-m-d', strtotime('+30 days'));
        
        // ID 1: High, Overdue, Long description/tags
        $longDesc = "Review security audit findings and write a 500-word summary of all vulnerabilities found in the latest deployment environment. This description is intentionally very long to test the dynamic column width calculation feature.";
        $id1 = $this->addTask($longDesc, self::PRIORITY_HIGH, $overdueDate, ['Security', 'Urgent', 'LongTag1']);
        echo "Added Task ID $id1 (High, Overdue).\n";
        
        // ID 2: Medium, Due tomorrow, for Tag Management test
        $id2 = $this->addTask("Write documentation for filtering feature.", self::PRIORITY_MEDIUM, $tomorrowDate, ['Documentation', 'Testing']);
        echo "Added Task ID $id2 (Medium, Due: $tomorrowDate).\n";
        
        // ID 3: Low, No Due Date, for Hard Delete test
        $id3 = $this->addTask("Buy coffee filters. This description is long.", self::PRIORITY_LOW, null, ['Personal']);
        echo "Added Task ID $id3 (Low, No Due Date).\n";
        
        // ID 4: High, Far Future, for Time to Completion test
        $id4 = $this->addTask("Refactor persistence layer.", self::PRIORITY_HIGH, $farFutureDate, ['Code', 'Urgent']);
        echo "Added Task ID $id4 (High, Due: $farFutureDate).\n";
        
        // ID 5: Low, Pending, for Purge test
        $id5 = $this->addTask("Test task for multiple status filter and archiving.", self::PRIORITY_LOW, $tomorrowDate, ['Test']);
        echo "Added Task ID $id5 (Low, Pending).\n";
        
        // ID 6: Medium, for Tag Exclusion test
        $id6 = $this->addTask("Task to be excluded by filter.", self::PRIORITY_MEDIUM, null, ['Exclude', 'Test']);
        echo "Added Task ID $id6 (Medium, Pending).\n";
        
        // ID 7: Task for Analytics calculation (1 hour duration)
        $id7 = $this->generateId();
        $this->tasks[$id7] = new Task(
            $id7, 
            "Analytics Test 1 (1 hour)", 
            self::PRIORITY_LOW, 
            null, 
            self::STATUS_COMPLETED, 
            '2024-01-01 10:00:00', 
            '2024-01-01 11:00:00' // 3600s
        );
        echo "Added Task ID $id7 (Completed, 1 hour duration).\n";
        
        // ID 8: Task for Analytics calculation (4 hours duration)
        $id8 = $this->generateId();
        $this->tasks[$id8] = new Task(
            $id8, 
            "Analytics Test 2 (4 hours)", 
            self::PRIORITY_LOW, 
            null, 
            self::STATUS_COMPLETED, 
            '2024-01-02 10:00:00', 
            '2024-01-02 14:00:00' // 14400s
        );
        echo "Added Task ID $id8 (Completed, 4 hour duration).\n";
        $this->saveTasks();
        // Total tasks: 8. Completed tasks: 2 (ID 7, 8). ID 4 will be completed later.

        // 2b. Test Strict Validation (addTask)
        echo "\n[2b] Testing Strict Validation (addTask - Expected: Exception for invalid priority):\n";
        try {
            $this->addTask("Task with invalid priority", "SuperDuperHigh");
            echo "FAIL: Invalid priority was accepted.\n";
        } catch (\InvalidArgumentException $e) {
            echo "SUCCESS: Caught expected exception for invalid priority: " . $e->getMessage() . "\n";
        }


        // 3. Test Tag Management (NEW FEATURE)
        echo "\n[3] Testing Dedicated Tag Management:\n";
        
        // Add Tag
        if ($this->addTag($id2, "PHP")) {
            echo "SUCCESS: Added 'php' tag to Task ID $id2.\n";
        } else {
            echo "FAIL: Could not add 'php' tag to Task ID $id2.\n";
        }
        
        // Remove Tag
        if ($this->removeTag($id2, "Testing")) {
            echo "SUCCESS: Removed 'Testing' tag from Task ID $id2.\n";
        } else {
            echo "FAIL: Could not remove 'Testing' tag to Task ID $id2.\n";
        }
        
        // Check current tags for ID 2 (Expected: Documentation, php)
        $task2 = $this->tasks[$id2];
        $expectedTags = ['documentation', 'php'];
        if (empty(array_diff($task2->tags, $expectedTags)) && empty(array_diff($expectedTags, $task2->tags))) {
            echo "SUCCESS: Task ID $id2 tags verified.\n";
        } else {
            echo "FAIL: Task ID $id2 tags incorrect. Found: " . implode(', ', $task2->tags) . "\n";
        }


        // 4. Test Update Task Error Handling (IMPROVEMENT)
        echo "\n[4] Testing Update Task Exception Handling (Expected: Exception for invalid status):\n";
        try {
            $this->updateTask($id2, ['status' => 'NonExistentStatus']);
            echo "FAIL: Invalid status was accepted by updateTask.\n";
        } catch (\InvalidArgumentException $e) {
            echo "SUCCESS: Caught expected exception for invalid status in updateTask: " . $e->getMessage() . "\n";
        }
        
        // 4b. Test Time to Completion Setup (ID 4)
        echo "\n[4b] Setting up Time to Completion test (ID 4):\n";
        $this->markTaskStatus($id4, self::STATUS_COMPLETED); 
        $timeToComplete = $this->getTimeToCompletion($id4);
        if ($timeToComplete) {
            echo "SUCCESS: Time to Completion calculated for ID 4: $timeToComplete (Expected: ~1 second).\n";
        } else {
            echo "FAIL: Time to Completion calculation failed for ID 4.\n";
        }
        
        // 4c. Test Task Details View (Should show Time to Completion)
        echo "\n[4c] Testing Detailed View for ID 4:\n";
        echo $this->getTaskDetails($id4);


        // 5. Test Hard Delete and Purge (NEW FEATURE)
        echo "\n[5] Testing Hard Delete and Purge:\n";
        
        // Soft delete ID 5 (Archived, for Purge)
        $this->deleteTask($id5); 
        echo "Task ID $id5 archived.\n";
        
        // Hard delete ID 3
        if ($this->hardDeleteTask($id3)) {
            echo "SUCCESS: Task ID $id3 permanently deleted.\n";
        } else {
            echo "FAIL: Task ID $id3 hard deletion failed.\n";
        }
        
        // Verify ID 3 is gone
        if (!isset($this->tasks[$id3])) {
            echo "SUCCESS: Task ID $id3 verified as deleted.\n";
        } else {
            echo "FAIL: Task ID $id3 still exists.\n";
        }
        
        // Purge archived tasks (Should delete ID 5)
        $purged = $this->purgeArchivedTasks();
        if ($purged === 1 && !isset($this->tasks[$id5])) {
            echo "SUCCESS: Purged 1 archived task (ID 5).\n";
        } else {
            echo "FAIL: Purge failed. Purged count: $purged. Task ID 5 exists: " . (isset($this->tasks[$id5]) ? 'Yes' : 'No') . "\n";
        }


        // 6. Test Advanced Filtering (Tag Exclusion) (NEW FEATURE)
        echo "\n[6] Testing Tag Exclusion Filtering:\n";
        
        // Filter: Show all tasks EXCEPT those with 'Exclude' tag (Should exclude ID 6)
        $activeTasks = $this->viewTasks([], 'id', 'asc', true); // Current tasks: 1, 2, 4, 6, 7, 8 (ID 3, 5 deleted)
        echo "Total tasks before exclusion filter: " . count($activeTasks) . "\n";
        
        $excludedTasks = $this->viewTasks(['tag_exclude' => 'Exclude'], 'id', 'asc', true);
        echo "Tasks after excluding 'Exclude' tag: " . count($excludedTasks) . " (Expected 5: ID 1, 2, 4, 7, 8)\n";
        
        $idsFound = array_map(fn($t) => $t->id, $excludedTasks);
        if (count($excludedTasks) === 5 && !in_array(6, $idsFound)) {
            echo "SUCCESS: Tag exclusion filter worked correctly.\n";
        } else {
            echo "FAIL: Tag exclusion filter failed. IDs found: " . implode(', ', $idsFound) . "\n";
        }
        
        echo $this->formatTasksForDisplay($excludedTasks);


        // 7. Final Check: Test Aggregate Analytics (v8.0 Advanced Analytics)
        echo "\n[7] Final Summary (Testing Advanced Analytics v8.0):\n";
        $summaryOutput = $this->getSummary();
        echo $summaryOutput;
        
        // Expected Analytics (Total 6 tasks: 1, 2, 4, 6, 7, 8): 
        // Completed tasks: 3 (ID 4, 7, 8). 
        // Completion Rate: 3/6 = 50.00%
        // Avg duration: ~6000s (1h 40m)
        // Med duration: 3600s (1h)
        // Priority: High 33.3%, Medium 33.3%, Low 33.3%
        
        $success = true;

        if (strpos($summaryOutput, "Completion Rate: 50.00%") === false) {
            echo "FAIL: Completion Rate calculation incorrect.\n";
            $success = false;
        }
        
        if (strpos($summaryOutput, "Avg. Completion Time: 1h 40m") === false) {
            echo "FAIL: Avg. Completion Time calculation incorrect.\n";
            $success = false;
        }
        
        // NEW ASSERTION: Median Time
        if (strpos($summaryOutput, "Med. Completion Time: 1h") === false) {
            echo "FAIL: Median Completion Time calculation incorrect (Expected 1h).\n";
            $success = false;
        }
        
        // NEW ASSERTION: Priority Distribution (Checking High and Low)
        // Note: We must account for the ANSI color codes surrounding the priority name in the output string.
        // Output format: "  - [COLOR]Priority[RESET]: [COUNT] tasks ([PERCENT])"
        
        // We search for the full colored string segment.
        
        $highNeedle = self::COLOR_RED . "High  " . self::COLOR_RESET . ":    2 tasks ( 33.3%)";
        if (strpos($summaryOutput, $highNeedle) === false) {
            echo "FAIL: Priority Distribution (High) incorrect (Expected High 2 tasks ( 33.3%)).\n";
            $success = false;
        }
        
        $lowNeedle = self::COLOR_RESET . "Low   " . self::COLOR_RESET . ":    2 tasks ( 33.3%)";
        if (strpos($summaryOutput, $lowNeedle) === false) {
            echo "FAIL: Priority Distribution (Low) incorrect (Expected Low 2 tasks ( 33.3%)).\n";
            $success = false;
        }

        if ($success) {
            echo "SUCCESS: Advanced Analytics verified.\n";
        }

        // Final cleanup
        if (file_exists($storageFile)) {
             unlink($storageFile);
        }

        echo "\n==================================================\n";
        echo "--- Task Manager Ultimate Hybrid Self Test Completed (v8.0) ---\n";
        echo "==================================================\n";
    }
}

/**
 * Class Task (Dedicated object for task data)
 * Represents a single task item, using DateTimeImmutable for dates.
 */
class Task {
    public ?\DateTimeImmutable $dueDate = null; 
    public ?\DateTimeImmutable $completedAt = null; 
    public array $tags = []; 

    public readonly \DateTimeImmutable $createdAt;

    /**
     * @param int $id
     * @param string $description
     * @param string $priority
     * @param string|null $dueDateString
     * @param string $status
     * @param string $createdAtString
     * @param string|null $completedAtString
     * @param array<string>|null $tags
     */
    public function __construct(
        public readonly int $id, 
        public string $description, 
        public string $priority, 
        ?string $dueDateString, 
        public string $status, 
        string $createdAtString, 
        ?string $completedAtString = null, 
        ?array $tags = [] 
    ) {
        // Initialization of readonly $createdAt
        try {
            $this->createdAt = new \DateTimeImmutable($createdAtString);
        } catch (\Throwable $e) {
            $this->createdAt = new \DateTimeImmutable(); // Fallback to now
        }
        
        $this->tags = $tags ?? [];
        
        // Set due date using the dedicated setter for validation
        $this->setDueDate($dueDateString);
        
        // Set completed date
        if ($completedAtString) {
            try {
                $this->completedAt = new \DateTimeImmutable($completedAtString);
            } catch (\Throwable $e) {
                $this->completedAt = null;
            }
        }
    }
    
    /**
     * Sets the due date from a string, handling null or invalid inputs.
     */
    public function setDueDate(?string $dueDateString): void {
        $this->dueDate = null;
        if ($dueDateString) {
            try {
                // Only store YYYY-MM-DD date part, setting time to midnight for consistent comparison
                $this->dueDate = (new \DateTimeImmutable($dueDateString))->setTime(0, 0, 0);
            } catch (\Throwable $e) {
                // Invalid date string, keep as null
            }
        }
    }
    
    /**
     * Checks if the task is overdue (not completed/archived and due date is in the past).
     */
    public function isOverdue(): bool {
        if ($this->status === TaskManager::STATUS_COMPLETED || $this->status === TaskManager::STATUS_ARCHIVED) {
            return false;
        }
        if (!$this->dueDate) {
            return false;
        }
        
        // Compare against the start of today (midnight)
        $today = new \DateTimeImmutable('today');
        return $this->dueDate < $today;
    }

    /**
     * Helper function to convert object to array for saving
     * @return array<string, mixed>
     */
    public function toArray(): array {
        $data = get_object_vars($this);
        
        // Convert DateTimeImmutable objects to string representations for persistence
        $data['createdAt'] = $this->createdAt->format('Y-m-d H:i:s');
        $data['dueDate'] = $this->dueDate ? $this->dueDate->format('Y-m-d') : null;
        $data['completedAt'] = $this->completedAt ? $this->completedAt->format('Y-m-d H:i:s') : null;
        
        return $data;
    }
    
    /**
     * Static method to create Task from array (for loading)
     * @throws \InvalidArgumentException If essential data is missing or invalid.
     */
    public static function fromArray(array $data): self {
        
        // Strict checks
        $id = (int)($data['id'] ?? 0);
        if ($id <= 0) {
            throw new \InvalidArgumentException("Task ID is missing or invalid during deserialization.");
        }
        
        $description = trim(strip_tags($data['description'] ?? ''));
        if (empty($description)) {
             throw new \InvalidArgumentException("Task description is missing during deserialization.");
        }
        
        $priority = $data['priority'] ?? TaskManager::PRIORITY_MEDIUM;
        $status = $data['status'] ?? TaskManager::STATUS_PENDING;
        
        // Basic validation for core fields during load
        if (!in_array($priority, TaskManager::VALID_PRIORITIES, true)) {
            $priority = TaskManager::PRIORITY_MEDIUM;
        }
        if (!in_array($status, TaskManager::VALID_STATUSES, true)) {
            $status = TaskManager::STATUS_PENDING;
        }
        
        // Ensure tags are loaded as lowercase
        $tags = $data['tags'] ?? [];
        if (!empty($tags)) {
            $tags = array_map('strtolower', $tags);
        }
        
        return new self(
            $id,
            $description,
            $priority,
            $data['dueDate'] ?? null, 
            $status,
            $data['createdAt'] ?? date('Y-m-d H:i:s'), 
            $data['completedAt'] ?? null, 
            $tags
        );
    }
}
// --- Execution block: CLI Handler (v8.0) ---
if (php_sapi_name() == 'cli') {
    $manager = new TaskManager();
    
    // Check if the script is run with arguments
    if (isset($argv[1])) {
        $command = strtolower($argv[1]);
        
        try {
            switch ($command) {
                case 'add':
                    // Use getopt for optional flags
                    $options = getopt("", ["priority:", "due:", "tags:"]);
                    
                    // Find the first argument that is not an option or the command itself
                    $description = null;
                    // Note: $argv[0] is script name, $argv[1] is command. Start search from index 2.
                    $foundDesc = false;
                    foreach ($argv as $index => $arg) {
                        if ($index > 1 && strpos($arg, '--') !== 0) {
                            $description = $arg;
                            $foundDesc = true;
                            break;
                        }
                    }
                    
                    if (!$foundDesc) {
                        echo "Error: Description is required for 'add'.\n";
                        echo $manager->showHelp();
                        exit(1);
                    }
                    
                    $priority = $options['priority'] ?? TaskManager::PRIORITY_MEDIUM;
                    $dueDate = $options['due'] ?? null;
                    $tags = isset($options['tags']) ? explode(',', $options['tags']) : [];
                    
                    $id = $manager->addTask($description, $priority, $dueDate, $tags);
                    echo "Task added successfully with ID: $id\n";
                    break;

                case 'view':
                    // Use getopt for optional flags
                    $options = getopt("", ["status:", "tag:", "tag_exclude:", "sort:", "order:", "completed", "due_before:", "due_after:"]);
                    
                    $filters = [];
                    // Status filter allows multiple comma-separated values
                    if (isset($options['status'])) {
                        $filters['status'] = explode(',', $options['status']); 
                    }
                    // Tag filter uses the 'tags' filter key
                    if (isset($options['tag'])) {
                        $filters['tags'] = $options['tag']; 
                    }
                    // NEW: Tag exclusion filter
                    if (isset($options['tag_exclude'])) {
                        $filters['tag_exclude'] = $options['tag_exclude']; 
                    }
                    if (isset($options['due_before'])) {
                        $filters['due_before'] = $options['due_before']; 
                    }
                    if (isset($options['due_after'])) {
                        $filters['due_after'] = $options['due_after']; 
                    }
                    
                    $sortBy = $options['sort'] ?? 'id';
                    $sortOrder = $options['order'] ?? 'asc';
                    // If --completed is NOT set, we hide completed AND archived tasks.
                    $showCompleted = isset($options['completed']); 
                    
                    $tasks = $manager->viewTasks($filters, $sortBy, $sortOrder, $showCompleted);
                    echo $manager->formatTasksForDisplay($tasks);
                    break;
                    
                case 'show':
                    $id = $argv[2] ?? null;
                    if (!$id || !is_numeric($id)) {
                        echo "Error: Task ID is required for 'show'.\n";
                        echo $manager->showHelp();
                        exit(1);
                    }
                    
                    echo $manager->getTaskDetails((int)$id);
                    break;

                case 'update':
                    $id = $argv[2] ?? null;
                    if (!$id || !is_numeric($id)) {
                        echo "Error: Task ID is required for 'update'.\n";
                        echo $manager->showHelp();
                        exit(1);
                    }
                    
                    $options = getopt("", ["desc:", "status:", "priority:", "due:", "tags:"]);
                    $updates = [];
                    
                    if (isset($options['desc'])) $updates['description'] = $options['desc'];
                    if (isset($options['status'])) $updates['status'] = $options['status'];
                    if (isset($options['priority'])) $updates['priority'] = $options['priority'];
                    if (isset($options['due'])) $updates['dueDate'] = $options['due'];
                    if (isset($options['tags'])) $updates['tags'] = explode(',', $options['tags']);
                    
                    if (empty($updates)) {
                        echo "Error: No updates provided.\n";
                        echo $manager->showHelp();
                        exit(1);
                    }
                    
                    if ($manager->updateTask((int)$id, $updates)) {
                        echo "Task ID $id updated successfully.\n";
                    } else {
                        // This case should now only handle Task ID not found, as invalid status/priority throws exception
                        echo "Error: Task ID $id not found.\n";
                    }
                    break;

                case 'status':
                    $id = $argv[2] ?? null;
                    $newStatus = $argv[3] ?? null;

                    if (!$id || !is_numeric($id) || !$newStatus) {
                        echo "Error: Task ID and new status are required for 'status'.\n";
                        echo $manager->showHelp();
                        exit(1);
                    }
                    
                    // markTaskStatus now throws InvalidArgumentException for invalid status
                    if ($manager->markTaskStatus((int)$id, $newStatus)) {
                        echo "Task ID $id status set to $newStatus.\n";
                    } else {
                        echo "Error: Task ID $id not found.\n";
                    }
                    break;
                    
                case 'tag-add': // NEW FEATURE
                    $id = $argv[2] ?? null;
                    $tag = $argv[3] ?? null;

                    if (!$id || !is_numeric($id) || !$tag) {
                        echo "Error: Task ID and tag are required for 'tag-add'.\n";
                        echo $manager->showHelp();
                        exit(1);
                    }
                    
                    if ($manager->addTag((int)$id, $tag)) {
                        echo "Tag '$tag' added to Task ID $id.\n";
                    } else {
                        echo "Error: Task ID $id not found or tag '$tag' already exists.\n";
                    }
                    break;
                    
                case 'tag-remove': // NEW FEATURE
                    $id = $argv[2] ?? null;
                    $tag = $argv[3] ?? null;

                    if (!$id || !is_numeric($id) || !$tag) {
                        echo "Error: Task ID and tag are required for 'tag-remove'.\n";
                        echo $manager->showHelp();
                        exit(1);
                    }
                    
                    if ($manager->removeTag((int)$id, $tag)) {
                        echo "Tag '$tag' removed from Task ID $id.\n";
                    } else {
                        echo "Error: Task ID $id not found or tag '$tag' not present.\n";
                    }
                    break;
                    
                case 'delete':
                    $id = $argv[2] ?? null;
                    if (!$id || !is_numeric($id)) {
                        echo "Error: Task ID is required for 'delete'.\n";
                        echo $manager->showHelp();
                        exit(1);
                    }
                    
                    if ($manager->deleteTask((int)$id)) {
                        echo "Task ID $id archived successfully.\n";
                    } else {
                        echo "Error: Task ID $id not found.\n";
                    }
                    break;
                    
                case 'hard-delete': // NEW FEATURE
                    $id = $argv[2] ?? null;
                    if (!$id || !is_numeric($id)) {
                        echo "Error: Task ID is required for 'hard-delete'.\n";
                        echo $manager->showHelp();
                        exit(1);
                    }
                    
                    if ($manager->hardDeleteTask((int)$id)) {
                        echo "Task ID $id permanently deleted.\n";
                    } else {
                        echo "Error: Task ID $id not found.\n";
                    }
                    break;

                case 'purge': // NEW FEATURE
                    $purgedCount = $manager->purgeArchivedTasks();
                    if ($purgedCount > 0) {
                        echo "Successfully purged $purgedCount archived tasks.\n";
                    } else {
                        echo "No archived tasks to purge.\n";
                    }
                    break;
                    
                case 'summary':
                    echo $manager->getSummary();
                    break;
                    
                case 'tags': 
                    $uniqueTags = $manager->getUniqueTags();
                    if (empty($uniqueTags)) {
                        echo "No tags found across all tasks.\n";
                    } else {
                        echo "\n--- Unique Tags ---\n";
                        echo implode(', ', $uniqueTags) . "\n";
                        echo "-------------------\n";
                    }
                    break;
                    
                case 'test':
                    $manager->self_test();
                    break;

                case 'help':
                default:
                    echo $manager->showHelp();
                    break;
            }
        } catch (\InvalidArgumentException $e) {
             // Catches exceptions thrown by addTask, updateTask, and markTaskStatus for invalid data
             echo "Input Error: " . $e->getMessage() . "\n";
             exit(1);
        } catch (\Throwable $e) {
            echo "An unexpected error occurred: " . $e->getMessage() . "\n";
            exit(1);
        }
        
    } else {
        // No command provided, default to help
        echo $manager->showHelp();
    }
}
?>
