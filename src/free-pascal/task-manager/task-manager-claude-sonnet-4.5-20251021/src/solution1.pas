
program solution1;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, taskmanager;

var
  manager: TTaskManager;
  task: TTask;
  aiReport, timeReport: string;

begin
  WriteLn('========================================');
  WriteLn('HYBRID TASK MANAGER DEMONSTRATION');
  WriteLn('(AI + Time Tracking Combined)');
  WriteLn('========================================');
  WriteLn;
  
  manager := TTaskManager.Create;
  try
    WriteLn('🧠 + ⏱️  Running hybrid self-test...');
    WriteLn;
    manager.SelfTest;
    WriteLn;
    
    WriteLn('========================================');
    WriteLn('🚀 CREATING DEMO PROJECT');
    WriteLn('========================================');
    WriteLn;
    
    manager.Clear;
    
    { Create some completed tasks for AI learning }
    task := manager.AddTask('Previous sprint: Backend API');
    task.EstimatedHours := 12;
    task.Category := 'Development';
    task.Priority := tpHigh;
    task.Status := tsCompleted;
    task.CompletedDate := Now - 7;
    task.AssignedTo := 'Alice';
    { Simulate tracked time }
    task.AddTimeEntry(Now - 7.5, Now - 7.0, 'API implementation');
    task.AddTimeEntry(Now - 6.5, Now - 6.0, 'Testing');
    WriteLn('Created historical task for AI learning');
    
    { Create current project tasks }
    task := manager.AddTask('Database schema design');
    task.Description := 'Design and implement database schema';
    task.Priority := tpHigh;
    task.Category := 'Development';
    task.EstimatedHours := 8;
    task.DueDate := IncDay(Now, 3);
    task.AssignedTo := 'Alice';
    WriteLn('Created: ' + task.Title);
    
    { Start timing this task }
    manager.StartTask(task.Id, 'Beginning design work');
    WriteLn('  ⏱️  Started timing...');
    Sleep(50); { Simulate some work }
    manager.StopTask(task.Id);
    WriteLn(Format('  ⏱️  Tracked %.4f hours', [task.GetTotalTrackedHours]));
    
    task := manager.AddTask('API endpoint implementation');
    task.Description := 'Develop REST API endpoints';
    task.Priority := tpCritical;
    task.Category := 'Development';
    task.EstimatedHours := 20;
    task.DueDate := IncDay(Now, 5);
    task.Status := tsInProgress;
    task.AssignedTo := 'Alice';
    task.AddDependency(1);
    WriteLn('Created: ' + task.Title);
    
    task := manager.AddTask('Frontend components');
    task.Description := 'Build React components for UI';
    task.Priority := tpHigh;
    task.Category := 'Development';
    task.EstimatedHours := 16;
    task.DueDate := IncDay(Now, 7);
    task.AssignedTo := 'Bob';
    WriteLn('Created: ' + task.Title);
    
    task := manager.AddTask('Integration testing');
    task.Description := 'End-to-end integration tests';
    task.Priority := tpCritical;
    task.Category := 'QA';
    task.EstimatedHours := 12;
    task.DueDate := IncDay(Now, 2);
    task.AssignedTo := 'Alice';
    task.AddDependency(2);
    task.AddDependency(3);
    WriteLn('Created: ' + task.Title);
    
    WriteLn;
    WriteLn(Format('✓ Created %d tasks in project', [manager.GetTaskCount - 1]));
    WriteLn;
    
    WriteLn('========================================');
    WriteLn('📊 FULL HYBRID REPORT');
    WriteLn('========================================');
    WriteLn(manager.GenerateReport);
    
    WriteLn('========================================');
    WriteLn('🧠 AI ANALYSIS REPORT');
    WriteLn('========================================');
    aiReport := manager.GenerateAIReport;
    WriteLn(aiReport);
    
    WriteLn('========================================');
    WriteLn('⏱️  TIME TRACKING REPORT');
    WriteLn('========================================');
    timeReport := manager.GenerateTimeTrackingReport;
    WriteLn(timeReport);
    
    WriteLn('========================================');
    WriteLn('💾 SAVING HYBRID PROJECT');
    WriteLn('========================================');
    if manager.SaveToFile('solution2/hybrid_project.dat') then
      WriteLn('✓ Saved hybrid project (V3 format with AI + Time Tracking)')
    else
      WriteLn('✗ Failed to save project');
    WriteLn;
    
    WriteLn('========================================');
    WriteLn('🎉 HYBRID DEMONSTRATION COMPLETE!');
    WriteLn('========================================');
    WriteLn;
    WriteLn('This hybrid solution demonstrates:');
    WriteLn('  ✓ Automated time tracking with start/stop');
    WriteLn('  ✓ AI-powered deadline predictions');
    WriteLn('  ✓ Machine learning from tracked data');
    WriteLn('  ✓ Conflict detection and risk assessment');
    WriteLn('  ✓ Smart scheduling and workload analysis');
    WriteLn('  ✓ Productivity metrics and insights');
    WriteLn;
    WriteLn('The AI learns from TRACKED time data, making');
    WriteLn('predictions more accurate than manual estimates!');
    WriteLn;
    
  finally
    manager.Free;
  end;
end.
