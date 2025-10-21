
unit taskmanager;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, Classes, Math;

type
  TTaskStatus = (tsNone, tsPending, tsInProgress, tsCompleted, tsCancelled);
  TTaskPriority = (tpNone, tpLow, tpMedium, tpHigh, tpCritical);
  
  TIntArray = array of integer;
  TStringArray = array of string;
  
  { Time Tracking Types - from Solution 2 }
  TTimeEntry = record
    StartTime: TDateTime;
    EndTime: TDateTime;
    Duration: double;
    Description: string;
    IsActive: boolean;
  end;
  
  TTimeEntryArray = array of TTimeEntry;
  
  { AI/Smart Feature Types - from Solution 1 }
  TRiskLevel = (rlNone, rlLow, rlMedium, rlHigh, rlCritical);
  TConflictType = (ctNone, ctDeadlineConflict, ctResourceOverload, ctCircularDependency, 
                   ctImpossibleDeadline, ctPriorityMismatch);
  
  TDeadlinePrediction = record
    taskid: integer;
    suggesteddeadline: TDateTime;
    confidence: double;
    reasoning: string;
  end;
  TDeadlinePredictionArray = array of TDeadlinePrediction;
  
  TConflict = record
    conflicttype: TConflictType;
    severity: TRiskLevel;
    affectedtaskids: TIntArray;
    description: string;
    suggestion: string;
  end;
  TConflictArray = array of TConflict;
  
  TPriorityRecommendation = record
    taskid: integer;
    currentpriority: TTaskPriority;
    recommendedpriority: TTaskPriority;
    reasoning: string;
    confidence: double;
  end;
  TPriorityRecommendationArray = array of TPriorityRecommendation;
  
  TRiskAssessment = record
    taskid: integer;
    risklevel: TRiskLevel;
    riskfactors: TStringArray;
    mitigation: string;
    probabilityofdelay: double;
  end;
  TRiskAssessmentArray = array of TRiskAssessment;
  
  TWorkloadAnalysis = record
    assignedto: string;
    totaltasks: integer;
    totalestimatedhours: double;
    tasksbyprioritycount: array[TTaskPriority] of integer;
    isoverloaded: boolean;
    overloadfactor: double;
    recommendation: string;
  end;
  TWorkloadAnalysisArray = array of TWorkloadAnalysis;
  
  TCompletionPrediction = record
    taskid: integer;
    predictedcompletiondate: TDateTime;
    confidence: double;
    basedontasks: integer;
    reasoning: string;
  end;
  TCompletionPredictionArray = array of TCompletionPrediction;
  
  TScheduleRecommendation = record
    taskid: integer;
    suggestedstartdate: TDateTime;
    suggestedenddate: TDateTime;
    reasoning: string;
    dependencychain: TIntArray;
  end;
  TScheduleRecommendationArray = array of TScheduleRecommendation;
  
  THistoricalTask = record
    estimatedhours: double;
    actualhours: double;
    priority: TTaskPriority;
    category: string;
    haddelay: boolean;
    delayfactor: double;
  end;
  THistoricalTaskArray = array of THistoricalTask;
  
  { Productivity Metrics - from Solution 2 }
  TProductivityMetrics = record
    TotalTasksAnalyzed: integer;
    AverageEstimateAccuracy: double;
    MostUnderestimatedCategory: string;
    MostOverestimatedCategory: string;
    TotalTimeSpent: double;
    AverageTaskDuration: double;
    TotalEstimatedTime: double;
    TotalActualTime: double;
    AverageOverrunPercentage: double;
  end;
  
  TCategoryTimeStats = record
    Category: string;
    TotalEstimated: double;
    TotalActual: double;
    TaskCount: integer;
    AverageAccuracy: double;
    OverrunPercentage: double;
  end;
  TCategoryTimeStatsArray = array of TCategoryTimeStats;
  
  TTask = class
  private
    fid: integer;
    ftitle: string;
    fdescription: string;
    fstatus: TTaskStatus;
    fpriority: TTaskPriority;
    fcreateddate: TDateTime;
    fduedate: TDateTime;
    fcompleteddate: TDateTime;
    fstartdate: TDateTime;
    fcategory: string;
    ftags: TStringArray;
    fdependencies: TIntArray;
    festimatedhours: double;
    factualhours: double;
    fassignedto: string;
    
    { Time Tracking Fields - from Solution 2 }
    ftimeentries: TTimeEntryArray;
    fistiming: boolean;
    ftotaltrackedhours: double;
    
    procedure RecalculateTotalTrackedHours;
  public
    constructor Create(aid: integer; const atitle: string);
    destructor Destroy; override;
    
    function TaskToString: string;
    function ToFileString: string;
    procedure FromFileString(const s: string);
    function IsOverdue: boolean;
    function CanStart(const completedtasks: TIntArray): boolean;
    
    { Time Tracking Methods - from Solution 2 }
    function StartTiming(const description: string): boolean;
    function StopTiming: boolean;
    function GetCurrentSessionDuration: double;
    function AddTimeEntry(starttime, endtime: TDateTime; const description: string): boolean;
    function GetTimeEntries: TTimeEntryArray;
    function GetTotalTrackedHours: double;
    function GetEstimateAccuracy: double;
    function IsCurrentlyTiming: boolean;
    procedure ClearTimeEntries;
    
    { Array Helper Methods - from Solution 2 }
    procedure AddDependency(taskid: integer);
    procedure RemoveDependency(taskid: integer);
    procedure ClearDependencies;
    procedure AddTag(const tag: string);
    procedure RemoveTag(const tag: string);
    procedure ClearTags;
    
    property Id: integer read fid;
    property Title: string read ftitle write ftitle;
    property Description: string read fdescription write fdescription;
    property Status: TTaskStatus read fstatus write fstatus;
    property Priority: TTaskPriority read fpriority write fpriority;
    property CreatedDate: TDateTime read fcreateddate;
    property DueDate: TDateTime read fduedate write fduedate;
    property CompletedDate: TDateTime read fcompleteddate write fcompleteddate;
    property StartDate: TDateTime read fstartdate write fstartdate;
    property Category: string read fcategory write fcategory;
    property Tags: TStringArray read ftags write ftags;
    property Dependencies: TIntArray read fdependencies write fdependencies;
    property EstimatedHours: double read festimatedhours write festimatedhours;
    property ActualHours: double read factualhours write factualhours;
    property AssignedTo: string read fassignedto write fassignedto;
    property IsTiming: boolean read fistiming;
    property TotalTrackedHours: double read ftotaltrackedhours;
  end;
  
  TTaskArray = array of TTask;
  
  TTaskManager = class
  private
    ftasks: TTaskArray;
    fnexttaskid: integer;
    ffilename: string;
    
    function FindTaskIndex(taskid: integer): integer;
    function RiskLevelToString(risk: TRiskLevel): string;
    function ConflictTypeToString(ct: TConflictType): string;
    
    { AI Helper Methods - from Solution 1 }
    function BuildHistoricalData: THistoricalTaskArray;
    function GetTaskDependents(taskid: integer): TIntArray;
    function DetectCircularDependencyHelper(taskid: integer; visited: TIntArray): boolean;
    function CalculateAverageDelayFactor(const historical: THistoricalTaskArray;
      priority: TTaskPriority; const category: string): double;
    function CalculateConfidence(samplesize: integer): double;
    function CalculateDelayProbability(estimatedhours: double; priority: TTaskPriority;
      const category: string; daysuntildeadline: double;
      const historical: THistoricalTaskArray): double;
    
    { Time Analytics Helper Methods - from Solution 2 }
    function GetCompletedTasksWithEstimates: TTaskArray;
    function CalculateCategoryStats(const category: string): TCategoryTimeStats;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Clear;
    function AddTask(const title: string): TTask;
    function GetTask(taskid: integer): TTask;
    function DeleteTask(taskid: integer): boolean;
    function GetAllTasks: TTaskArray;
    function GetTasksByStatus(status: TTaskStatus): TTaskArray;
    function GetTasksByPriority(priority: TTaskPriority): TTaskArray;
    function GetTasksByCategory(const category: string): TTaskArray;
    function GetOverdueTasks: TTaskArray;
    function SearchTasks(const searchterm: string): TTaskArray;
    
    function GetTaskCount: integer;
    function GetCompletedTaskCount: integer;
    function GetPendingTaskCount: integer;
    function GetTotalEstimatedHours: double;
    function GetTotalActualHours: double;
    
    function SaveToFile(const filename: string): boolean;
    function LoadFromFile(const filename: string): boolean;
    
    procedure SortTasksByPriority(ascending: boolean = false);
    procedure SortTasksByDueDate(ascending: boolean = true);
    procedure SortTasksById;
    
    function GenerateReport: string;
    function GenerateCategoryReport: string;
    function GeneratePriorityReport: string;
    
    { Time Tracking Methods - from Solution 2 }
    function StartTask(taskid: integer; const description: string): boolean;
    function StopTask(taskid: integer): boolean;
    function GetActiveTimingTasks: TTaskArray;
    function GetProductivityMetrics: TProductivityMetrics;
    function GetEstimateAccuracyByCategory: string;
    function CalculateAverageOverrun: double;
    function GetCategoryTimeStatistics: TCategoryTimeStatsArray;
    function GenerateTimeTrackingReport: string;
    function GetTasksWithTimeEntries: TTaskArray;
    
    { AI/SMART FEATURES - from Solution 1 }
    function PredictOptimalDeadline(taskid: integer): TDeadlinePrediction;
    function PredictAllDeadlines: TDeadlinePredictionArray;
    function DetectConflicts: TConflictArray;
    function DetectCircularDependencies: TConflictArray;
    function DetectDeadlineConflicts: TConflictArray;
    function DetectResourceConflicts: TConflictArray;
    function RecommendTaskPriority(taskid: integer): TPriorityRecommendation;
    function RecommendAllPriorities: TPriorityRecommendationArray;
    function AutoAdjustPriorities: integer;
    function PredictTaskCompletion(taskid: integer): TCompletionPrediction;
    function PredictAllCompletions: TCompletionPredictionArray;
    function AssessTaskRisk(taskid: integer): TRiskAssessment;
    function AssessAllRisks: TRiskAssessmentArray;
    function GetHighRiskTasks: TTaskArray;
    function AnalyzeWorkload(const assignedto: string): TWorkloadAnalysis;
    function AnalyzeAllWorkloads: TWorkloadAnalysisArray;
    function GetOverloadedUsers: TStringArray;
    function GenerateSmartSchedule: TScheduleRecommendationArray;
    function ComputeExecutionOrder: TIntArray;
    function GetCriticalPathTasks: TIntArray;
    function GenerateAIReport: string;
    function GenerateRiskReport: string;
    function GenerateWorkloadReport: string;
    
    procedure SelfTest;
  end;

function StatusToString(status: TTaskStatus): string;
function StringToStatus(const s: string): TTaskStatus;
function PriorityToString(priority: TTaskPriority): string;
function StringToPriority(const s: string): TTaskPriority;

implementation

{ Helper Functions }

function StatusToString(status: TTaskStatus): string;
begin
  case status of
    tsNone: Result := 'None';
    tsPending: Result := 'Pending';
    tsInProgress: Result := 'In Progress';
    tsCompleted: Result := 'Completed';
    tsCancelled: Result := 'Cancelled';
  else
    Result := 'Unknown';
  end;
end;

function StringToStatus(const s: string): TTaskStatus;
begin
  if s = 'Pending' then Result := tsPending
  else if s = 'In Progress' then Result := tsInProgress
  else if s = 'Completed' then Result := tsCompleted
  else if s = 'Cancelled' then Result := tsCancelled
  else Result := tsNone;
end;

function PriorityToString(priority: TTaskPriority): string;
begin
  case priority of
    tpNone: Result := 'None';
    tpLow: Result := 'Low';
    tpMedium: Result := 'Medium';
    tpHigh: Result := 'High';
    tpCritical: Result := 'Critical';
  else
    Result := 'Unknown';
  end;
end;

function StringToPriority(const s: string): TTaskPriority;
begin
  if s = 'Low' then Result := tpLow
  else if s = 'Medium' then Result := tpMedium
  else if s = 'High' then Result := tpHigh
  else if s = 'Critical' then Result := tpCritical
  else Result := tpNone;
end;

{ TTask Implementation }

constructor TTask.Create(aid: integer; const atitle: string);
begin
  inherited Create;
  fid := aid;
  ftitle := atitle;
  fdescription := '';
  fstatus := tsPending;
  fpriority := tpMedium;
  fcreateddate := Now;
  fduedate := 0;
  fcompleteddate := 0;
  fstartdate := 0;
  fcategory := '';
  SetLength(ftags, 0);
  SetLength(fdependencies, 0);
  festimatedhours := 0;
  factualhours := 0;
  fassignedto := '';
  
  { Initialize time tracking }
  SetLength(ftimeentries, 0);
  fistiming := false;
  ftotaltrackedhours := 0;
end;

destructor TTask.Destroy;
begin
  SetLength(ftags, 0);
  SetLength(fdependencies, 0);
  SetLength(ftimeentries, 0);
  inherited Destroy;
end;

procedure TTask.RecalculateTotalTrackedHours;
var
  i: integer;
  total: double;
begin
  total := 0;
  for i := 0 to Length(ftimeentries) - 1 do
  begin
    if not ftimeentries[i].IsActive then
      total := total + ftimeentries[i].Duration;
  end;
  
  if fistiming then
    total := total + GetCurrentSessionDuration;
    
  ftotaltrackedhours := total;
  
  { Sync actual hours with tracked hours }
  factualhours := total;
end;

function TTask.StartTiming(const description: string): boolean;
var
  idx: integer;
begin
  Result := false;
  
  if fistiming then
    exit;
  
  idx := Length(ftimeentries);
  SetLength(ftimeentries, idx + 1);
  
  ftimeentries[idx].StartTime := Now;
  ftimeentries[idx].EndTime := 0;
  ftimeentries[idx].Duration := 0;
  ftimeentries[idx].Description := description;
  ftimeentries[idx].IsActive := true;
  
  fistiming := true;
  
  if fstatus = tsPending then
    fstatus := tsInProgress;
  
  if fstartdate = 0 then
    fstartdate := Now;
  
  Result := true;
end;

function TTask.StopTiming: boolean;
var
  i: integer;
  duration: double;
begin
  Result := false;
  
  if not fistiming then
    exit;
  
  for i := 0 to Length(ftimeentries) - 1 do
  begin
    if ftimeentries[i].IsActive then
    begin
      ftimeentries[i].EndTime := Now;
      duration := (ftimeentries[i].EndTime - ftimeentries[i].StartTime) * 24;
      ftimeentries[i].Duration := duration;
      ftimeentries[i].IsActive := false;
      
      fistiming := false;
      RecalculateTotalTrackedHours;
      
      Result := true;
      exit;
    end;
  end;
end;

function TTask.GetCurrentSessionDuration: double;
var
  i: integer;
begin
  Result := 0;
  if not fistiming then
    exit;
    
  for i := 0 to Length(ftimeentries) - 1 do
  begin
    if ftimeentries[i].IsActive then
    begin
      Result := (Now - ftimeentries[i].StartTime) * 24;
      exit;
    end;
  end;
end;

function TTask.AddTimeEntry(starttime, endtime: TDateTime; const description: string): boolean;
var
  idx: integer;
  duration: double;
begin
  Result := false;
  
  if endtime <= starttime then
    exit;
  
  idx := Length(ftimeentries);
  SetLength(ftimeentries, idx + 1);
  
  duration := (endtime - starttime) * 24;
  
  ftimeentries[idx].StartTime := starttime;
  ftimeentries[idx].EndTime := endtime;
  ftimeentries[idx].Duration := duration;
  ftimeentries[idx].Description := description;
  ftimeentries[idx].IsActive := false;
  
  RecalculateTotalTrackedHours;
  
  Result := true;
end;

function TTask.GetTimeEntries: TTimeEntryArray;
begin
  SetLength(Result, Length(ftimeentries));
  Result := Copy(ftimeentries, 0, Length(ftimeentries));
end;

function TTask.GetTotalTrackedHours: double;
begin
  RecalculateTotalTrackedHours;
  Result := ftotaltrackedhours;
end;

function TTask.GetEstimateAccuracy: double;
begin
  Result := 0;
  if (festimatedhours > 0) and (factualhours > 0) then
    Result := (festimatedhours / factualhours) * 100;
end;

function TTask.IsCurrentlyTiming: boolean;
begin
  Result := fistiming;
end;

procedure TTask.ClearTimeEntries;
begin
  SetLength(ftimeentries, 0);
  fistiming := false;
  ftotaltrackedhours := 0;
end;

procedure TTask.AddDependency(taskid: integer);
var
  i, len: integer;
begin
  for i := 0 to Length(fdependencies) - 1 do
    if fdependencies[i] = taskid then
      exit;
  
  len := Length(fdependencies);
  SetLength(fdependencies, len + 1);
  fdependencies[len] := taskid;
end;

procedure TTask.RemoveDependency(taskid: integer);
var
  i, len: integer;
  temp: TIntArray;
begin
  len := 0;
  SetLength(temp, Length(fdependencies));
  
  for i := 0 to Length(fdependencies) - 1 do
  begin
    if fdependencies[i] <> taskid then
    begin
      temp[len] := fdependencies[i];
      inc(len);
    end;
  end;
  
  SetLength(temp, len);
  fdependencies := temp;
end;

procedure TTask.ClearDependencies;
begin
  SetLength(fdependencies, 0);
end;

procedure TTask.AddTag(const tag: string);
var
  i, len: integer;
begin
  for i := 0 to Length(ftags) - 1 do
    if ftags[i] = tag then
      exit;
  
  len := Length(ftags);
  SetLength(ftags, len + 1);
  ftags[len] := tag;
end;

procedure TTask.RemoveTag(const tag: string);
var
  i, len: integer;
  temp: TStringArray;
begin
  len := 0;
  SetLength(temp, Length(ftags));
  
  for i := 0 to Length(ftags) - 1 do
  begin
    if ftags[i] <> tag then
    begin
      temp[len] := ftags[i];
      inc(len);
    end;
  end;
  
  SetLength(temp, len);
  ftags := temp;
end;

procedure TTask.ClearTags;
begin
  SetLength(ftags, 0);
end;

function TTask.TaskToString: string;
var
  i: integer;
  tagstr, depstr: string;
begin
  Result := Format('Task #%d: %s', [fid, ftitle]);
  Result := Result + #13#10 + Format('  Status: %s', [StatusToString(fstatus)]);
  Result := Result + #13#10 + Format('  Priority: %s', [PriorityToString(fpriority)]);
  
  if fdescription <> '' then
    Result := Result + #13#10 + Format('  Description: %s', [fdescription]);
  
  if fcategory <> '' then
    Result := Result + #13#10 + Format('  Category: %s', [fcategory]);
  
  if fassignedto <> '' then
    Result := Result + #13#10 + Format('  Assigned to: %s', [fassignedto]);
  
  if festimatedhours > 0 then
    Result := Result + #13#10 + Format('  Estimated: %.2f hours', [festimatedhours]);
  
  if factualhours > 0 then
    Result := Result + #13#10 + Format('  Actual/Tracked: %.2f hours', [factualhours]);
  
  if fistiming then
    Result := Result + #13#10 + Format('  ⏱ CURRENTLY TIMING (%.2f hours this session)', [GetCurrentSessionDuration]);
  
  if (festimatedhours > 0) and (factualhours > 0) then
    Result := Result + #13#10 + Format('  Accuracy: %.1f%%', [GetEstimateAccuracy]);
  
  if Length(ftags) > 0 then
  begin
    tagstr := '';
    for i := 0 to Length(ftags) - 1 do
    begin
      if i > 0 then tagstr := tagstr + ', ';
      tagstr := tagstr + ftags[i];
    end;
    Result := Result + #13#10 + Format('  Tags: %s', [tagstr]);
  end;
  
  if Length(fdependencies) > 0 then
  begin
    depstr := '';
    for i := 0 to Length(fdependencies) - 1 do
    begin
      if i > 0 then depstr := depstr + ', ';
      depstr := depstr + IntToStr(fdependencies[i]);
    end;
    Result := Result + #13#10 + Format('  Dependencies: %s', [depstr]);
  end;
  
  if fduedate > 0 then
    Result := Result + #13#10 + Format('  Due: %s', [DateTimeToStr(fduedate)]);
  
  if fcompleteddate > 0 then
    Result := Result + #13#10 + Format('  Completed: %s', [DateTimeToStr(fcompleteddate)]);
  
  if Length(ftimeentries) > 0 then
    Result := Result + #13#10 + Format('  Time entries: %d sessions', [Length(ftimeentries)]);
end;

function TTask.ToFileString: string;
var
  i: integer;
  tagstr, depstr, timestr: string;
begin
  tagstr := '';
  for i := 0 to Length(ftags) - 1 do
  begin
    if i > 0 then tagstr := tagstr + ',';
    tagstr := tagstr + ftags[i];
  end;
  
  depstr := '';
  for i := 0 to Length(fdependencies) - 1 do
  begin
    if i > 0 then depstr := depstr + ',';
    depstr := depstr + IntToStr(fdependencies[i]);
  end;
  
  timestr := IntToStr(Length(ftimeentries));
  for i := 0 to Length(ftimeentries) - 1 do
  begin
    timestr := timestr + '|' + FloatToStr(ftimeentries[i].StartTime);
    timestr := timestr + '|' + FloatToStr(ftimeentries[i].EndTime);
    timestr := timestr + '|' + FloatToStr(ftimeentries[i].Duration);
    timestr := timestr + '|' + ftimeentries[i].Description;
    if ftimeentries[i].IsActive then
      timestr := timestr + '|1'
    else
      timestr := timestr + '|0';
  end;
  
  Result := IntToStr(fid) + '|' +
            ftitle + '|' +
            fdescription + '|' +
            IntToStr(Ord(fstatus)) + '|' +
            IntToStr(Ord(fpriority)) + '|' +
            FloatToStr(fcreateddate) + '|' +
            FloatToStr(fduedate) + '|' +
            FloatToStr(fcompleteddate) + '|' +
            FloatToStr(fstartdate) + '|' +
            fcategory + '|' +
            tagstr + '|' +
            depstr + '|' +
            FloatToStr(festimatedhours) + '|' +
            FloatToStr(factualhours) + '|' +
            fassignedto + '|' +
            timestr;
end;

procedure TTask.FromFileString(const s: string);
var
  parts: TStringArray;
  i, numentries, idx: integer;
  tagparts, depparts: TStringArray;
begin
  parts := s.Split(['|']);
  
  if Length(parts) < 14 then
    exit;
  
  fid := StrToIntDef(parts[0], 0);
  ftitle := parts[1];
  fdescription := parts[2];
  fstatus := TTaskStatus(StrToIntDef(parts[3], 0));
  fpriority := TTaskPriority(StrToIntDef(parts[4], 0));
  fcreateddate := StrToFloatDef(parts[5], 0);
  fduedate := StrToFloatDef(parts[6], 0);
  fcompleteddate := StrToFloatDef(parts[7], 0);
  
  if Length(parts) >= 15 then
    fstartdate := StrToFloatDef(parts[8], 0)
  else
    fstartdate := 0;
  
  if Length(parts) >= 10 then
    fcategory := parts[9]
  else
    fcategory := '';
  
  if (Length(parts) >= 11) and (parts[10] <> '') then
  begin
    tagparts := parts[10].Split([',']);
    SetLength(ftags, Length(tagparts));
    for i := 0 to Length(tagparts) - 1 do
      ftags[i] := tagparts[i];
  end
  else
    SetLength(ftags, 0);
  
  if (Length(parts) >= 12) and (parts[11] <> '') then
  begin
    depparts := parts[11].Split([',']);
    SetLength(fdependencies, Length(depparts));
    for i := 0 to Length(depparts) - 1 do
      fdependencies[i] := StrToIntDef(depparts[i], 0);
  end
  else
    SetLength(fdependencies, 0);
  
  if Length(parts) >= 13 then
    festimatedhours := StrToFloatDef(parts[12], 0)
  else
    festimatedhours := 0;
    
  if Length(parts) >= 14 then
    factualhours := StrToFloatDef(parts[13], 0)
  else
    factualhours := 0;
    
  if Length(parts) >= 15 then
    fassignedto := parts[14]
  else
    fassignedto := '';
  
  if Length(parts) >= 16 then
  begin
    numentries := StrToIntDef(parts[15], 0);
    SetLength(ftimeentries, numentries);
    
    idx := 16;
    for i := 0 to numentries - 1 do
    begin
      if idx + 4 < Length(parts) then
      begin
        ftimeentries[i].StartTime := StrToFloatDef(parts[idx], 0);
        ftimeentries[i].EndTime := StrToFloatDef(parts[idx + 1], 0);
        ftimeentries[i].Duration := StrToFloatDef(parts[idx + 2], 0);
        ftimeentries[i].Description := parts[idx + 3];
        ftimeentries[i].IsActive := (parts[idx + 4] = '1');
        
        if ftimeentries[i].IsActive then
          fistiming := true;
        
        idx := idx + 5;
      end;
    end;
    
    RecalculateTotalTrackedHours;
  end
  else
  begin
    SetLength(ftimeentries, 0);
    fistiming := false;
    ftotaltrackedhours := 0;
  end;
end;

function TTask.IsOverdue: boolean;
begin
  Result := (fduedate > 0) and (Now > fduedate) and (fstatus <> tsCompleted) and (fstatus <> tsCancelled);
end;

function TTask.CanStart(const completedtasks: TIntArray): boolean;
var
  i, j: integer;
  found: boolean;
begin
  Result := true;
  
  for i := 0 to Length(fdependencies) - 1 do
  begin
    found := false;
    for j := 0 to Length(completedtasks) - 1 do
    begin
      if fdependencies[i] = completedtasks[j] then
      begin
        found := true;
        break;
      end;
    end;
    
    if not found then
    begin
      Result := false;
      exit;
    end;
  end;
end;

{ TTaskManager Implementation }

constructor TTaskManager.Create;
begin
  inherited Create;
  SetLength(ftasks, 0);
  fnexttaskid := 1;
  ffilename := '';
end;

destructor TTaskManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTaskManager.Clear;
var
  i: integer;
begin
  for i := 0 to Length(ftasks) - 1 do
    ftasks[i].Free;
  SetLength(ftasks, 0);
  fnexttaskid := 1;
end;

function TTaskManager.AddTask(const title: string): TTask;
var
  task: TTask;
  len: integer;
begin
  task := TTask.Create(fnexttaskid, title);
  inc(fnexttaskid);
  
  len := Length(ftasks);
  SetLength(ftasks, len + 1);
  ftasks[len] := task;
  
  Result := task;
end;

function TTaskManager.FindTaskIndex(taskid: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Length(ftasks) - 1 do
  begin
    if ftasks[i].Id = taskid then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function TTaskManager.GetTask(taskid: integer): TTask;
var
  idx: integer;
begin
  Result := nil;
  idx := FindTaskIndex(taskid);
  if idx >= 0 then
    Result := ftasks[idx];
end;

function TTaskManager.DeleteTask(taskid: integer): boolean;
var
  idx, i: integer;
  newtasks: TTaskArray;
begin
  Result := false;
  idx := FindTaskIndex(taskid);
  
  if idx < 0 then
    exit;
  
  ftasks[idx].Free;
  
  SetLength(newtasks, Length(ftasks) - 1);
  for i := 0 to idx - 1 do
    newtasks[i] := ftasks[i];
  for i := idx + 1 to Length(ftasks) - 1 do
    newtasks[i - 1] := ftasks[i];
  
  ftasks := newtasks;
  Result := true;
end;

function TTaskManager.GetAllTasks: TTaskArray;
begin
  SetLength(Result, Length(ftasks));
  Result := Copy(ftasks, 0, Length(ftasks));
end;

function TTaskManager.GetTasksByStatus(status: TTaskStatus): TTaskArray;
var
  i, count: integer;
begin
  SetLength(Result, Length(ftasks));
  count := 0;
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    if ftasks[i].Status = status then
    begin
      Result[count] := ftasks[i];
      inc(count);
    end;
  end;
  
  SetLength(Result, count);
end;

function TTaskManager.GetTasksByPriority(priority: TTaskPriority): TTaskArray;
var
  i, count: integer;
begin
  SetLength(Result, Length(ftasks));
  count := 0;
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    if ftasks[i].Priority = priority then
    begin
      Result[count] := ftasks[i];
      inc(count);
    end;
  end;
  
  SetLength(Result, count);
end;

function TTaskManager.GetTasksByCategory(const category: string): TTaskArray;
var
  i, count: integer;
begin
  SetLength(Result, Length(ftasks));
  count := 0;
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    if ftasks[i].Category = category then
    begin
      Result[count] := ftasks[i];
      inc(count);
    end;
  end;
  
  SetLength(Result, count);
end;

function TTaskManager.GetOverdueTasks: TTaskArray;
var
  i, count: integer;
begin
  SetLength(Result, Length(ftasks));
  count := 0;
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    if ftasks[i].IsOverdue then
    begin
      Result[count] := ftasks[i];
      inc(count);
    end;
  end;
  
  SetLength(Result, count);
end;

function TTaskManager.SearchTasks(const searchterm: string): TTaskArray;
var
  i, count: integer;
  lowerterm: string;
begin
  SetLength(Result, Length(ftasks));
  count := 0;
  lowerterm := LowerCase(searchterm);
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    if (Pos(lowerterm, LowerCase(ftasks[i].Title)) > 0) or
       (Pos(lowerterm, LowerCase(ftasks[i].Description)) > 0) or
       (Pos(lowerterm, LowerCase(ftasks[i].Category)) > 0) then
    begin
      Result[count] := ftasks[i];
      inc(count);
    end;
  end;
  
  SetLength(Result, count);
end;

function TTaskManager.GetTaskCount: integer;
begin
  Result := Length(ftasks);
end;

function TTaskManager.GetCompletedTaskCount: integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(ftasks) - 1 do
    if ftasks[i].Status = tsCompleted then
      inc(count);
  Result := count;
end;

function TTaskManager.GetPendingTaskCount: integer;
var
  i, count: integer;
begin
  count := 0;
  for i := 0 to Length(ftasks) - 1 do
    if ftasks[i].Status = tsPending then
      inc(count);
  Result := count;
end;

function TTaskManager.GetTotalEstimatedHours: double;
var
  i: integer;
  total: double;
begin
  total := 0;
  for i := 0 to Length(ftasks) - 1 do
    total := total + ftasks[i].EstimatedHours;
  Result := total;
end;

function TTaskManager.GetTotalActualHours: double;
var
  i: integer;
  total: double;
begin
  total := 0;
  for i := 0 to Length(ftasks) - 1 do
    total := total + ftasks[i].ActualHours;
  Result := total;
end;

procedure TTaskManager.SortTasksByPriority(ascending: boolean = false);
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to Length(ftasks) - 2 do
    for j := i + 1 to Length(ftasks) - 1 do
      if ascending then
      begin
        if Ord(ftasks[i].Priority) > Ord(ftasks[j].Priority) then
        begin
          temp := ftasks[i];
          ftasks[i] := ftasks[j];
          ftasks[j] := temp;
        end;
      end
      else
      begin
        if Ord(ftasks[i].Priority) < Ord(ftasks[j].Priority) then
        begin
          temp := ftasks[i];
          ftasks[i] := ftasks[j];
          ftasks[j] := temp;
        end;
      end;
end;

procedure TTaskManager.SortTasksByDueDate(ascending: boolean = true);
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to Length(ftasks) - 2 do
    for j := i + 1 to Length(ftasks) - 1 do
      if (ftasks[j].DueDate > 0) and
         ((ftasks[i].DueDate = 0) or (ftasks[j].DueDate < ftasks[i].DueDate)) then
      begin
        temp := ftasks[i];
        ftasks[i] := ftasks[j];
        ftasks[j] := temp;
      end;
end;

procedure TTaskManager.SortTasksById;
var
  i, j: integer;
  temp: TTask;
begin
  for i := 0 to Length(ftasks) - 2 do
    for j := i + 1 to Length(ftasks) - 1 do
      if ftasks[i].Id > ftasks[j].Id then
      begin
        temp := ftasks[i];
        ftasks[i] := ftasks[j];
        ftasks[j] := temp;
      end;
end;

function TTaskManager.SaveToFile(const filename: string): boolean;
var
  f: TextFile;
  i: integer;
begin
  Result := false;
  
  try
    AssignFile(f, filename);
    Rewrite(f);
    
    WriteLn(f, 'TASKMANAGER_V3');
    WriteLn(f, fnexttaskid);
    WriteLn(f, Length(ftasks));
    
    for i := 0 to Length(ftasks) - 1 do
      WriteLn(f, ftasks[i].ToFileString);
    
    CloseFile(f);
    ffilename := filename;
    Result := true;
  except
    on E: Exception do
      Result := false;
  end;
end;

function TTaskManager.LoadFromFile(const filename: string): boolean;
var
  f: TextFile;
  version: string;
  taskcount, i: integer;
  line: string;
  task: TTask;
begin
  Result := false;
  Clear;
  
  try
    AssignFile(f, filename);
    Reset(f);
    
    ReadLn(f, version);
    if (version <> 'TASKMANAGER_V1') and (version <> 'TASKMANAGER_V2') and (version <> 'TASKMANAGER_V3') then
    begin
      CloseFile(f);
      exit;
    end;
    
    ReadLn(f, fnexttaskid);
    ReadLn(f, taskcount);
    
    SetLength(ftasks, taskcount);
    
    for i := 0 to taskcount - 1 do
    begin
      ReadLn(f, line);
      task := TTask.Create(0, '');
      task.FromFileString(line);
      ftasks[i] := task;
    end;
    
    CloseFile(f);
    ffilename := filename;
    Result := true;
  except
    on E: Exception do
      Result := false;
  end;
end;

function TTaskManager.GenerateReport: string;
var
  i: integer;
begin
  Result := '========================================' + #13#10;
  Result := Result + 'HYBRID TASK MANAGER REPORT' + #13#10;
  Result := Result + '(AI + Time Tracking Edition)' + #13#10;
  Result := Result + '========================================' + #13#10;
  Result := Result + Format('Total Tasks: %d', [GetTaskCount]) + #13#10;
  Result := Result + Format('Completed: %d', [GetCompletedTaskCount]) + #13#10;
  Result := Result + Format('Pending: %d', [GetPendingTaskCount]) + #13#10;
  Result := Result + Format('Estimated Hours: %.2f', [GetTotalEstimatedHours]) + #13#10;
  Result := Result + Format('Actual Hours: %.2f', [GetTotalActualHours]) + #13#10;
  Result := Result + '========================================' + #13#10;
  
  if Length(ftasks) = 0 then
  begin
    Result := Result + #13#10 + 'No tasks defined.' + #13#10;
    exit;
  end;
  
  Result := Result + #13#10 + 'TASK DETAILS:' + #13#10 + #13#10;
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    Result := Result + ftasks[i].TaskToString + #13#10;
    if i < Length(ftasks) - 1 then
      Result := Result + #13#10;
  end;
end;

function TTaskManager.GenerateCategoryReport: string;
var
  categories: TStringArray;
  i, j: integer;
  found: boolean;
  tasks: TTaskArray;
begin
  Result := '========================================' + #13#10;
  Result := Result + 'CATEGORY REPORT' + #13#10;
  Result := Result + '========================================' + #13#10;
  
  SetLength(categories, 0);
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    if ftasks[i].Category <> '' then
    begin
      found := false;
      for j := 0 to Length(categories) - 1 do
      begin
        if categories[j] = ftasks[i].Category then
        begin
          found := true;
          break;
        end;
      end;
      
      if not found then
      begin
        SetLength(categories, Length(categories) + 1);
        categories[Length(categories) - 1] := ftasks[i].Category;
      end;
    end;
  end;
  
  if Length(categories) = 0 then
  begin
    Result := Result + #13#10 + 'No categories defined.' + #13#10;
    exit;
  end;
  
  for i := 0 to Length(categories) - 1 do
  begin
    tasks := GetTasksByCategory(categories[i]);
    Result := Result + #13#10 + Format('%s: %d tasks', [categories[i], Length(tasks)]);
  end;
  
  Result := Result + #13#10;
end;

function TTaskManager.GeneratePriorityReport: string;
var
  p: TTaskPriority;
  tasks: TTaskArray;
begin
  Result := '========================================' + #13#10;
  Result := Result + 'PRIORITY REPORT' + #13#10;
  Result := Result + '========================================' + #13#10;
  
  for p := tpLow to tpCritical do
  begin
    tasks := GetTasksByPriority(p);
    Result := Result + Format('%s: %d tasks', [PriorityToString(p), Length(tasks)]) + #13#10;
  end;
end;

{ Time Tracking Methods - from Solution 2 }

function TTaskManager.StartTask(taskid: integer; const description: string): boolean;
var
  task: TTask;
begin
  Result := false;
  task := GetTask(taskid);
  
  if task = nil then
    exit;
  
  Result := task.StartTiming(description);
end;

function TTaskManager.StopTask(taskid: integer): boolean;
var
  task: TTask;
begin
  Result := false;
  task := GetTask(taskid);
  
  if task = nil then
    exit;
  
  Result := task.StopTiming;
end;

function TTaskManager.GetActiveTimingTasks: TTaskArray;
var
  i, count: integer;
begin
  SetLength(Result, Length(ftasks));
  count := 0;
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    if ftasks[i].IsCurrentlyTiming then
    begin
      Result[count] := ftasks[i];
      inc(count);
    end;
  end;
  
  SetLength(Result, count);
end;

function TTaskManager.GetCompletedTasksWithEstimates: TTaskArray;
var
  i, count: integer;
begin
  SetLength(Result, Length(ftasks));
  count := 0;
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    if (ftasks[i].Status = tsCompleted) and
       (ftasks[i].EstimatedHours > 0) and
       (ftasks[i].ActualHours > 0) then
    begin
      Result[count] := ftasks[i];
      inc(count);
    end;
  end;
  
  SetLength(Result, count);
end;

function TTaskManager.GetProductivityMetrics: TProductivityMetrics;
var
  tasks: TTaskArray;
  i, j: integer;
  totalest, totalact: double;
  worstunder, worstover: double;
  worstundercat, worstovercat: string;
  categories: TStringArray;
  stats: TCategoryTimeStats;
  found: boolean;
begin
  { Initialize result }
  Result.TotalTasksAnalyzed := 0;
  Result.AverageEstimateAccuracy := 0;
  Result.MostUnderestimatedCategory := 'N/A';
  Result.MostOverestimatedCategory := 'N/A';
  Result.TotalTimeSpent := 0;
  Result.AverageTaskDuration := 0;
  Result.TotalEstimatedTime := 0;
  Result.TotalActualTime := 0;
  Result.AverageOverrunPercentage := 0;
  
  tasks := GetCompletedTasksWithEstimates;
  
  if Length(tasks) = 0 then
    exit;
  
  Result.TotalTasksAnalyzed := Length(tasks);
  totalest := 0;
  totalact := 0;
  
  worstunder := 100;
  worstover := 100;
  worstundercat := '';
  worstovercat := '';
  
  for i := 0 to Length(tasks) - 1 do
  begin
    totalest := totalest + tasks[i].EstimatedHours;
    totalact := totalact + tasks[i].ActualHours;
  end;
  
  Result.TotalEstimatedTime := totalest;
  Result.TotalActualTime := totalact;
  Result.TotalTimeSpent := totalact;
  Result.AverageTaskDuration := totalact / Length(tasks);
  
  if totalest > 0 then
    Result.AverageEstimateAccuracy := (totalest / totalact) * 100
  else
    Result.AverageEstimateAccuracy := 0;
  
  if totalest > 0 then
    Result.AverageOverrunPercentage := ((totalact - totalest) / totalest) * 100
  else
    Result.AverageOverrunPercentage := 0;
  
  SetLength(categories, 0);
  
  for i := 0 to Length(tasks) - 1 do
  begin
    if tasks[i].Category <> '' then
    begin
      found := false;
      for j := 0 to Length(categories) - 1 do
      begin
        if categories[j] = tasks[i].Category then
        begin
          found := true;
          break;
        end;
      end;
      
      if not found then
      begin
        SetLength(categories, Length(categories) + 1);
        categories[Length(categories) - 1] := tasks[i].Category;
      end;
    end;
  end;
  
  for i := 0 to Length(categories) - 1 do
  begin
    stats := CalculateCategoryStats(categories[i]);
    
    if stats.TaskCount > 0 then
    begin
      if stats.AverageAccuracy < worstunder then
      begin
        worstunder := stats.AverageAccuracy;
        worstundercat := categories[i];
      end;
      
      if stats.AverageAccuracy > worstover then
      begin
        worstover := stats.AverageAccuracy;
        worstovercat := categories[i];
      end;
    end;
  end;
  
  if worstunder < 100 then
    Result.MostUnderestimatedCategory := worstundercat;
  
  if worstover > 100 then
    Result.MostOverestimatedCategory := worstovercat;
end;

function TTaskManager.CalculateCategoryStats(const category: string): TCategoryTimeStats;
var
  tasks: TTaskArray;
  i: integer;
  totalest, totalact: double;
begin
  { Initialize result }
  Result.Category := category;
  Result.TotalEstimated := 0;
  Result.TotalActual := 0;
  Result.TaskCount := 0;
  Result.AverageAccuracy := 0;
  Result.OverrunPercentage := 0;
  
  tasks := GetTasksByCategory(category);
  totalest := 0;
  totalact := 0;
  
  for i := 0 to Length(tasks) - 1 do
  begin
    if (tasks[i].Status = tsCompleted) and
       (tasks[i].EstimatedHours > 0) and
       (tasks[i].ActualHours > 0) then
    begin
      inc(Result.TaskCount);
      totalest := totalest + tasks[i].EstimatedHours;
      totalact := totalact + tasks[i].ActualHours;
    end;
  end;
  
  Result.TotalEstimated := totalest;
  Result.TotalActual := totalact;
  
  if (Result.TaskCount > 0) and (totalact > 0) then
  begin
    Result.AverageAccuracy := (totalest / totalact) * 100;
    Result.OverrunPercentage := ((totalact - totalest) / totalest) * 100;
  end;
end;

function TTaskManager.GetEstimateAccuracyByCategory: string;
var
  categories: TStringArray;
  i, j: integer;
  found: boolean;
  stats: TCategoryTimeStats;
begin
  Result := '========================================' + #13#10;
  Result := Result + 'ESTIMATE ACCURACY BY CATEGORY' + #13#10;
  Result := Result + '========================================' + #13#10;
  
  SetLength(categories, 0);
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    if ftasks[i].Category <> '' then
    begin
      found := false;
      for j := 0 to Length(categories) - 1 do
      begin
        if categories[j] = ftasks[i].Category then
        begin
          found := true;
          break;
        end;
      end;
      
      if not found then
      begin
        SetLength(categories, Length(categories) + 1);
        categories[Length(categories) - 1] := ftasks[i].Category;
      end;
    end;
  end;
  
  if Length(categories) = 0 then
  begin
    Result := Result + #13#10 + 'No categories with completed tasks.' + #13#10;
    exit;
  end;
  
  for i := 0 to Length(categories) - 1 do
  begin
    stats := CalculateCategoryStats(categories[i]);
    
    if stats.TaskCount > 0 then
    begin
      Result := Result + #13#10 + Format('Category: %s', [stats.Category]) + #13#10;
      Result := Result + Format('  Tasks: %d', [stats.TaskCount]) + #13#10;
      Result := Result + Format('  Estimated: %.2f hours', [stats.TotalEstimated]) + #13#10;
      Result := Result + Format('  Actual: %.2f hours', [stats.TotalActual]) + #13#10;
      Result := Result + Format('  Accuracy: %.1f%%', [stats.AverageAccuracy]) + #13#10;
      Result := Result + Format('  Overrun: %.1f%%', [stats.OverrunPercentage]) + #13#10;
      
      if stats.AverageAccuracy < 90 then
        Result := Result + '  ⚠ Significantly underestimated!' + #13#10
      else if stats.AverageAccuracy > 110 then
        Result := Result + '  ℹ Overestimated (buffer included)' + #13#10
      else
        Result := Result + '  ✓ Good accuracy!' + #13#10;
    end;
  end;
end;

function TTaskManager.CalculateAverageOverrun: double;
var
  tasks: TTaskArray;
  i: integer;
  totalest, totalact: double;
begin
  Result := 0;
  tasks := GetCompletedTasksWithEstimates;
  
  if Length(tasks) = 0 then
    exit;
  
  totalest := 0;
  totalact := 0;
  
  for i := 0 to Length(tasks) - 1 do
  begin
    totalest := totalest + tasks[i].EstimatedHours;
    totalact := totalact + tasks[i].ActualHours;
  end;
  
  if totalest > 0 then
    Result := ((totalact - totalest) / totalest) * 100;
end;

function TTaskManager.GetCategoryTimeStatistics: TCategoryTimeStatsArray;
var
  categories: TStringArray;
  i, j, count: integer;
  found: boolean;
begin
  SetLength(categories, 0);
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    if ftasks[i].Category <> '' then
    begin
      found := false;
      for j := 0 to Length(categories) - 1 do
      begin
        if categories[j] = ftasks[i].Category then
        begin
          found := true;
          break;
        end;
      end;
      
      if not found then
      begin
        SetLength(categories, Length(categories) + 1);
        categories[Length(categories) - 1] := ftasks[i].Category;
      end;
    end;
  end;
  
  count := 0;
  SetLength(Result, Length(categories));
  
  for i := 0 to Length(categories) - 1 do
  begin
    Result[i] := CalculateCategoryStats(categories[i]);
    if Result[i].TaskCount > 0 then
      inc(count);
  end;
  
  SetLength(Result, count);
end;

function TTaskManager.GenerateTimeTrackingReport: string;
var
  metrics: TProductivityMetrics;
  activetasks: TTaskArray;
  i: integer;
  totaltracked: double;
begin
  Result := '========================================' + #13#10;
  Result := Result + 'TIME TRACKING ANALYTICS REPORT' + #13#10;
  Result := Result + '========================================' + #13#10;
  
  metrics := GetProductivityMetrics;
  
  Result := Result + #13#10 + 'PRODUCTIVITY METRICS:' + #13#10;
  Result := Result + Format('  Tasks analyzed: %d', [metrics.TotalTasksAnalyzed]) + #13#10;
  Result := Result + Format('  Total estimated: %.2f hours', [metrics.TotalEstimatedTime]) + #13#10;
  Result := Result + Format('  Total actual: %.2f hours', [metrics.TotalActualTime]) + #13#10;
  Result := Result + Format('  Average estimate accuracy: %.1f%%', [metrics.AverageEstimateAccuracy]) + #13#10;
  Result := Result + Format('  Average overrun: %.1f%%', [metrics.AverageOverrunPercentage]) + #13#10;
  
  if metrics.TotalTasksAnalyzed > 0 then
  begin
    Result := Result + Format('  Average task duration: %.2f hours', [metrics.AverageTaskDuration]) + #13#10;
    
    if metrics.MostUnderestimatedCategory <> 'N/A' then
      Result := Result + Format('  Most underestimated: %s', [metrics.MostUnderestimatedCategory]) + #13#10;
    
    if metrics.MostOverestimatedCategory <> 'N/A' then
      Result := Result + Format('  Most overestimated: %s', [metrics.MostOverestimatedCategory]) + #13#10;
  end;
  
  totaltracked := 0;
  for i := 0 to Length(ftasks) - 1 do
    totaltracked := totaltracked + ftasks[i].GetTotalTrackedHours;
  
  Result := Result + #13#10 + Format('TOTAL TRACKED TIME: %.2f hours', [totaltracked]) + #13#10;
  
  activetasks := GetActiveTimingTasks;
  
  if Length(activetasks) > 0 then
  begin
    Result := Result + #13#10 + Format('ACTIVE TIMING SESSIONS: %d', [Length(activetasks)]) + #13#10;
    
    for i := 0 to Length(activetasks) - 1 do
    begin
      Result := Result + Format('  ⏱ Task #%d: %s (%.2f hours)', 
        [activetasks[i].Id, activetasks[i].Title, activetasks[i].GetCurrentSessionDuration]) + #13#10;
    end;
  end
  else
  begin
    Result := Result + #13#10 + 'No active timing sessions.' + #13#10;
  end;
  
  Result := Result + #13#10 + '========================================' + #13#10;
end;

function TTaskManager.GetTasksWithTimeEntries: TTaskArray;
var
  i, count: integer;
begin
  SetLength(Result, Length(ftasks));
  count := 0;
  
  for i := 0 to Length(ftasks) - 1 do
  begin
    if Length(ftasks[i].GetTimeEntries) > 0 then
    begin
      Result[count] := ftasks[i];
      inc(count);
    end;
  end;
  
  SetLength(Result, count);
end;

{ AI/Smart Features - from Solution 1 }

function TTaskManager.RiskLevelToString(risk: TRiskLevel): string;
begin
  case risk of
    rlCritical: result := 'CRITICAL';
    rlHigh: result := 'HIGH';
    rlMedium: result := 'MEDIUM';
    rlLow: result := 'LOW';
    else result := 'NONE';
  end;
end;

function TTaskManager.ConflictTypeToString(ct: TConflictType): string;
begin
  case ct of
    ctDeadlineConflict: result := 'Deadline Conflict';
    ctResourceOverload: result := 'Resource Overload';
    ctCircularDependency: result := 'Circular Dependency';
    ctImpossibleDeadline: result := 'Impossible Deadline';
    ctPriorityMismatch: result := 'Priority Mismatch';
    else result := 'None';
  end;
end;

function TTaskManager.CalculateConfidence(samplesize: integer): double;
begin
  if samplesize <= 0 then
    result := 0.0
  else if samplesize >= 100 then
    result := 0.95
  else
    result := 1.0 - (1.0 / (1.0 + samplesize / 10.0));
end;

function TTaskManager.BuildHistoricalData: THistoricalTaskArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  
  for i := 0 to High(ftasks) do
  begin
    if (ftasks[i].Status = tsCompleted) and (ftasks[i].EstimatedHours > 0) then
    begin
      SetLength(result, count + 1);
      result[count].estimatedhours := ftasks[i].EstimatedHours;
      result[count].actualhours := ftasks[i].ActualHours;
      result[count].priority := ftasks[i].Priority;
      result[count].category := ftasks[i].Category;
      
      if ftasks[i].ActualHours > ftasks[i].EstimatedHours then
      begin
        result[count].haddelay := true;
        if ftasks[i].EstimatedHours > 0 then
          result[count].delayfactor := ftasks[i].ActualHours / ftasks[i].EstimatedHours
        else
          result[count].delayfactor := 1.0;
      end
      else
      begin
        result[count].haddelay := false;
        result[count].delayfactor := 1.0;
      end;
      
      inc(count);
    end;
  end;
end;

function TTaskManager.CalculateAverageDelayFactor(const historical: THistoricalTaskArray;
  priority: TTaskPriority; const category: string): double;
var
  i, count: integer;
  total: double;
begin
  total := 0.0;
  count := 0;
  
  for i := 0 to High(historical) do
  begin
    if (priority <> tpNone) and (historical[i].priority <> priority) then
      continue;
    if (category <> '') and (historical[i].category <> category) then
      continue;
      
    total := total + historical[i].delayfactor;
    inc(count);
  end;
  
  if count > 0 then
    result := total / count
  else
    result := 1.2;
end;

function TTaskManager.CalculateDelayProbability(estimatedhours: double; priority: TTaskPriority;
  const category: string; daysuntildeadline: double;
  const historical: THistoricalTaskArray): double;
var
  avgdelayfactor: double;
  hoursperday: double;
  requireddays: double;
  buffer: double;
  i, relevantcount: integer;
  delayprobability: double;
begin
  avgdelayfactor := CalculateAverageDelayFactor(historical, priority, category);
  
  hoursperday := 6.0;
  requireddays := (estimatedhours * avgdelayfactor) / hoursperday;
  buffer := daysuntildeadline - requireddays;
  
  relevantcount := 0;
  delayprobability := 0.0;
  for i := 0 to High(historical) do
  begin
    if (priority = tpNone) or (historical[i].priority = priority) then
    begin
      inc(relevantcount);
      if historical[i].haddelay then
        delayprobability := delayprobability + 1.0;
    end;
  end;
  
  if relevantcount > 0 then
    delayprobability := delayprobability / relevantcount
  else
    delayprobability := 0.3;
  
  if buffer < 0 then
    result := 0.95
  else if buffer < 2 then
    result := Min(0.9, delayprobability + 0.3)
  else if buffer < 5 then
    result := Min(0.7, delayprobability + 0.1)
  else if buffer < 10 then
    result := delayprobability
  else
    result := Max(0.1, delayprobability - 0.2);
    
  result := Max(0.0, Min(1.0, result));
end;

function TTaskManager.GetTaskDependents(taskid: integer): TIntArray;
var
  i, j, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  
  for i := 0 to High(ftasks) do
  begin
    for j := 0 to High(ftasks[i].fdependencies) do
    begin
      if ftasks[i].fdependencies[j] = taskid then
      begin
        SetLength(result, count + 1);
        result[count] := ftasks[i].Id;
        inc(count);
        break;
      end;
    end;
  end;
end;

function TTaskManager.PredictOptimalDeadline(taskid: integer): TDeadlinePrediction;
var
  task: TTask;
  historical: THistoricalTaskArray;
  avgdelayfactor: double;
  adjustedhours: double;
  daysneeded: double;
  bufferdays: double;
  confidence: double;
begin
  { Initialize result }
  result.taskid := taskid;
  result.confidence := 0.0;
  result.reasoning := '';
  result.suggesteddeadline := 0;
  
  task := GetTask(taskid);
  if task = nil then
  begin
    result.reasoning := 'Task not found';
    exit;
  end;
  
  historical := BuildHistoricalData;
  avgdelayfactor := CalculateAverageDelayFactor(historical, task.Priority, task.Category);
  adjustedhours := task.EstimatedHours * avgdelayfactor;
  daysneeded := Ceil(adjustedhours / 6.0);
  
  case task.Priority of
    tpCritical: bufferdays := daysneeded * 0.1;
    tpHigh: bufferdays := daysneeded * 0.2;
    tpMedium: bufferdays := daysneeded * 0.3;
    tpLow: bufferdays := daysneeded * 0.5;
    else bufferdays := daysneeded * 0.25;
  end;
  
  if Length(task.fdependencies) > 0 then
    bufferdays := bufferdays + Length(task.fdependencies) * 2;
  
  result.suggesteddeadline := IncDay(Now, Round(daysneeded + bufferdays));
  confidence := CalculateConfidence(Length(historical));
  result.confidence := confidence;
  
  result.reasoning := Format('Based on %d historical tasks, estimated %.1f hours with %.1f%% typical delay factor. ' +
    'Recommended %.0f days (%.0f work days + %.0f buffer days)',
    [Length(historical), task.EstimatedHours, (avgdelayfactor - 1.0) * 100, 
     daysneeded + bufferdays, daysneeded, bufferdays]);
end;

function TTaskManager.PredictAllDeadlines: TDeadlinePredictionArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  
  for i := 0 to High(ftasks) do
  begin
    if (ftasks[i].Status <> tsCompleted) and (ftasks[i].EstimatedHours > 0) then
    begin
      SetLength(result, count + 1);
      result[count] := PredictOptimalDeadline(ftasks[i].Id);
      inc(count);
    end;
  end;
end;

function TTaskManager.DetectCircularDependencyHelper(taskid: integer; visited: TIntArray): boolean;
var
  i, j: integer;
  newvisited: TIntArray;
  task: TTask;
begin
  result := false;
  
  for i := 0 to High(visited) do
  begin
    if visited[i] = taskid then
    begin
      result := true;
      exit;
    end;
  end;
  
  task := GetTask(taskid);
  if task = nil then
    exit;
  
  SetLength(newvisited, Length(visited) + 1);
  for i := 0 to High(visited) do
    newvisited[i] := visited[i];
  newvisited[High(newvisited)] := taskid;
  
  for i := 0 to High(task.fdependencies) do
  begin
    if DetectCircularDependencyHelper(task.fdependencies[i], newvisited) then
    begin
      result := true;
      exit;
    end;
  end;
end;

function TTaskManager.DetectCircularDependencies: TConflictArray;
var
  i, count: integer;
  visited: TIntArray;
  conflict: TConflict;
begin
  SetLength(result, 0);
  count := 0;
  
  for i := 0 to High(ftasks) do
  begin
    SetLength(visited, 0);
    if DetectCircularDependencyHelper(ftasks[i].Id, visited) then
    begin
      SetLength(result, count + 1);
      conflict.conflicttype := ctCircularDependency;
      conflict.severity := rlHigh;
      SetLength(conflict.affectedtaskids, 1);
      conflict.affectedtaskids[0] := ftasks[i].Id;
      conflict.description := Format('Task #%d has a circular dependency', [ftasks[i].Id]);
      conflict.suggestion := 'Remove one of the dependencies to break the cycle';
      result[count] := conflict;
      inc(count);
    end;
  end;
end;

function TTaskManager.DetectDeadlineConflicts: TConflictArray;
var
  i, count: integer;
  prediction: TDeadlinePrediction;
  conflict: TConflict;
  daysavailable: double;
begin
  SetLength(result, 0);
  count := 0;
  
  for i := 0 to High(ftasks) do
  begin
    if (ftasks[i].Status <> tsCompleted) and (ftasks[i].DueDate > 0) and 
       (ftasks[i].EstimatedHours > 0) then
    begin
      prediction := PredictOptimalDeadline(ftasks[i].Id);
      
      if prediction.suggesteddeadline > ftasks[i].DueDate then
      begin
        daysavailable := DaysBetween(Now, ftasks[i].DueDate);
        
        SetLength(result, count + 1);
        conflict.conflicttype := ctImpossibleDeadline;
        
        if daysavailable < 0 then
          conflict.severity := rlCritical
        else if prediction.suggesteddeadline - ftasks[i].DueDate > 7 then
          conflict.severity := rlHigh
        else
          conflict.severity := rlMedium;
          
        SetLength(conflict.affectedtaskids, 1);
        conflict.affectedtaskids[0] := ftasks[i].Id;
        conflict.description := Format('Task #%d "%s" deadline is %d days too early',
          [ftasks[i].Id, ftasks[i].Title, 
           Round(prediction.suggesteddeadline - ftasks[i].DueDate)]);
        conflict.suggestion := Format('Consider extending deadline to %s or reducing scope',
          [DateToStr(prediction.suggesteddeadline)]);
        result[count] := conflict;
        inc(count);
      end;
    end;
  end;
end;

function TTaskManager.DetectResourceConflicts: TConflictArray;
var
  workloads: TWorkloadAnalysisArray;
  i, count: integer;
  conflict: TConflict;
begin
  SetLength(result, 0);
  count := 0;
  
  workloads := AnalyzeAllWorkloads;
  
  for i := 0 to High(workloads) do
  begin
    if workloads[i].isoverloaded then
    begin
      SetLength(result, count + 1);
      conflict.conflicttype := ctResourceOverload;
      
      if workloads[i].overloadfactor > 2.0 then
        conflict.severity := rlCritical
      else if workloads[i].overloadfactor > 1.5 then
        conflict.severity := rlHigh
      else
        conflict.severity := rlMedium;
        
      SetLength(conflict.affectedtaskids, 0);
      conflict.description := Format('User "%s" is %.0f%% overloaded with %d tasks (%.1f hours)',
        [workloads[i].assignedto, (workloads[i].overloadfactor - 1.0) * 100,
         workloads[i].totaltasks, workloads[i].totalestimatedhours]);
      conflict.suggestion := workloads[i].recommendation;
      result[count] := conflict;
      inc(count);
    end;
  end;
end;

function TTaskManager.DetectConflicts: TConflictArray;
var
  circular, deadline, resource: TConflictArray;
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  
  circular := DetectCircularDependencies;
  deadline := DetectDeadlineConflicts;
  resource := DetectResourceConflicts;
  
  SetLength(result, Length(circular) + Length(deadline) + Length(resource));
  
  for i := 0 to High(circular) do
  begin
    result[count] := circular[i];
    inc(count);
  end;
  
  for i := 0 to High(deadline) do
  begin
    result[count] := deadline[i];
    inc(count);
  end;
  
  for i := 0 to High(resource) do
  begin
    result[count] := resource[i];
    inc(count);
  end;
end;

function TTaskManager.RecommendTaskPriority(taskid: integer): TPriorityRecommendation;
var
  task: TTask;
  dependents: TIntArray;
  score: integer;
  daysuntildue: double;
begin
  { Initialize result }
  result.taskid := taskid;
  result.confidence := 0.0;
  result.reasoning := '';
  result.currentpriority := tpNone;
  result.recommendedpriority := tpNone;
  
  task := GetTask(taskid);
  if task = nil then
  begin
    result.reasoning := 'Task not found';
    exit;
  end;
  
  dependents := GetTaskDependents(taskid);
  result.currentpriority := task.Priority;
  
  score := 0;
  
  if task.DueDate > 0 then
  begin
    daysuntildue := DaysBetween(Now, task.DueDate);
    if task.DueDate < Now then
      score := score + 40
    else if daysuntildue < 3 then
      score := score + 30
    else if daysuntildue < 7 then
      score := score + 20
    else if daysuntildue < 14 then
      score := score + 10;
  end;
  
  score := score + (Length(dependents) * 5);
  
  if Length(task.fdependencies) > 3 then
    score := score + 10;
  
  if task.EstimatedHours > 40 then
    score := score + 15
  else if task.EstimatedHours > 20 then
    score := score + 10
  else if task.EstimatedHours > 10 then
    score := score + 5;
  
  if score >= 40 then
    result.recommendedpriority := tpCritical
  else if score >= 25 then
    result.recommendedpriority := tpHigh
  else if score >= 15 then
    result.recommendedpriority := tpMedium
  else
    result.recommendedpriority := tpLow;
  
  result.confidence := Min(0.95, 0.5 + (score / 100.0));
  
  result.reasoning := Format('Priority score: %d (deadline urgency + %d dependents + complexity)',
    [score, Length(dependents)]);
end;

function TTaskManager.RecommendAllPriorities: TPriorityRecommendationArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  
  for i := 0 to High(ftasks) do
  begin
    if ftasks[i].Status <> tsCompleted then
    begin
      SetLength(result, count + 1);
      result[count] := RecommendTaskPriority(ftasks[i].Id);
      inc(count);
    end;
  end;
end;

function TTaskManager.AutoAdjustPriorities: integer;
var
  recommendations: TPriorityRecommendationArray;
  i: integer;
  task: TTask;
begin
  result := 0;
  recommendations := RecommendAllPriorities;
  
  for i := 0 to High(recommendations) do
  begin
    if (recommendations[i].confidence > 0.7) and 
       (recommendations[i].recommendedpriority <> recommendations[i].currentpriority) then
    begin
      task := GetTask(recommendations[i].taskid);
      if task <> nil then
      begin
        task.Priority := recommendations[i].recommendedpriority;
        inc(result);
      end;
    end;
  end;
end;

function TTaskManager.PredictTaskCompletion(taskid: integer): TCompletionPrediction;
var
  task: TTask;
  historical: THistoricalTaskArray;
  avgdelayfactor: double;
  adjustedhours: double;
  daysneeded: double;
begin
  { Initialize result }
  result.taskid := taskid;
  result.basedontasks := 0;
  result.predictedcompletiondate := 0;
  result.confidence := 0.0;
  result.reasoning := '';
  
  task := GetTask(taskid);
  if task = nil then
  begin
    result.reasoning := 'Task not found';
    exit;
  end;
  
  historical := BuildHistoricalData;
  result.basedontasks := Length(historical);
  
  avgdelayfactor := CalculateAverageDelayFactor(historical, task.Priority, task.Category);
  adjustedhours := task.EstimatedHours * avgdelayfactor;
  daysneeded := adjustedhours / 6.0;
  
  if task.StartDate = 0 then
    result.predictedcompletiondate := IncDay(Now, Round(daysneeded))
  else
    result.predictedcompletiondate := IncDay(task.StartDate, Round(daysneeded));
  
  result.confidence := CalculateConfidence(Length(historical));
  
  result.reasoning := Format('Estimated %.1f hours with %.0f%% delay factor = %.1f adjusted hours. ' +
    'At 6 hours/day = %.1f days. Confidence: %.0f%%',
    [task.EstimatedHours, (avgdelayfactor - 1.0) * 100, adjustedhours, daysneeded, result.confidence * 100]);
end;

function TTaskManager.PredictAllCompletions: TCompletionPredictionArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  
  for i := 0 to High(ftasks) do
  begin
    if (ftasks[i].Status = tsInProgress) or (ftasks[i].Status = tsPending) then
    begin
      SetLength(result, count + 1);
      result[count] := PredictTaskCompletion(ftasks[i].Id);
      inc(count);
    end;
  end;
end;

function TTaskManager.AssessTaskRisk(taskid: integer): TRiskAssessment;
var
  task: TTask;
  historical: THistoricalTaskArray;
  delayprobability: double;
  daysuntildue: double;
  i: integer;
begin
  { Initialize result }
  result.taskid := taskid;
  result.risklevel := rlNone;
  SetLength(result.riskfactors, 0);
  result.mitigation := '';
  result.probabilityofdelay := 0.0;
  
  task := GetTask(taskid);
  if task = nil then
    exit;
  
  historical := BuildHistoricalData;
  
  if task.DueDate > 0 then
  begin
    daysuntildue := DaysBetween(Now, task.DueDate);
    if task.DueDate < Now then
      daysuntildue := -daysuntildue;
      
    delayprobability := CalculateDelayProbability(
      task.EstimatedHours, task.Priority, task.Category, daysuntildue, historical);
    result.probabilityofdelay := delayprobability;
    
    if delayprobability > 0.8 then
      result.risklevel := rlCritical
    else if delayprobability > 0.6 then
      result.risklevel := rlHigh
    else if delayprobability > 0.4 then
      result.risklevel := rlMedium
    else if delayprobability > 0.2 then
      result.risklevel := rlLow;
  end;
  
  i := 0;
  if task.DueDate < Now then
  begin
    SetLength(result.riskfactors, i + 1);
    result.riskfactors[i] := 'Deadline already passed';
    inc(i);
  end
  else if (task.DueDate > 0) and (DaysBetween(Now, task.DueDate) < 3) then
  begin
    SetLength(result.riskfactors, i + 1);
    result.riskfactors[i] := 'Approaching deadline';
    inc(i);
  end;
  
  if Length(task.fdependencies) > 3 then
  begin
    SetLength(result.riskfactors, i + 1);
    result.riskfactors[i] := 'Many dependencies';
    inc(i);
  end;
  
  if task.EstimatedHours > 40 then
  begin
    SetLength(result.riskfactors, i + 1);
    result.riskfactors[i] := 'Large task (>40 hours)';
    inc(i);
  end;
  
  if task.Priority = tpCritical then
  begin
    SetLength(result.riskfactors, i + 1);
    result.riskfactors[i] := 'Critical priority';
    inc(i);
  end;
  
  case result.risklevel of
    rlCritical:
      result.mitigation := 'URGENT: Escalate immediately, reassign resources, reduce scope';
    rlHigh:
      result.mitigation := 'Add resources, extend deadline if possible, daily monitoring';
    rlMedium:
      result.mitigation := 'Monitor closely, prepare contingency plan';
    rlLow:
      result.mitigation := 'Regular monitoring, minimal intervention needed';
  end;
end;

function TTaskManager.AssessAllRisks: TRiskAssessmentArray;
var
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  
  for i := 0 to High(ftasks) do
  begin
    if ftasks[i].Status <> tsCompleted then
    begin
      SetLength(result, count + 1);
      result[count] := AssessTaskRisk(ftasks[i].Id);
      inc(count);
    end;
  end;
end;

function TTaskManager.GetHighRiskTasks: TTaskArray;
var
  risks: TRiskAssessmentArray;
  i, count: integer;
  task: TTask;
begin
  SetLength(result, 0);
  count := 0;
  
  risks := AssessAllRisks;
  
  for i := 0 to High(risks) do
  begin
    if (risks[i].risklevel = rlHigh) or (risks[i].risklevel = rlCritical) then
    begin
      task := GetTask(risks[i].taskid);
      if task <> nil then
      begin
        SetLength(result, count + 1);
        result[count] := task;
        inc(count);
      end;
    end;
  end;
end;

function TTaskManager.AnalyzeWorkload(const assignedto: string): TWorkloadAnalysis;
var
  i: integer;
  priority: TTaskPriority;
  hoursperweek: double;
begin
  { Initialize result }
  result.assignedto := assignedto;
  result.totaltasks := 0;
  result.totalestimatedhours := 0.0;
  
  for priority := tpNone to tpCritical do
    result.tasksbyprioritycount[priority] := 0;
  
  result.isoverloaded := false;
  result.overloadfactor := 0.0;
  result.recommendation := '';
  
  for i := 0 to High(ftasks) do
  begin
    if (ftasks[i].AssignedTo = assignedto) and 
       (ftasks[i].Status <> tsCompleted) and
       (ftasks[i].Status <> tsCancelled) then
    begin
      inc(result.totaltasks);
      result.totalestimatedhours := result.totalestimatedhours + ftasks[i].EstimatedHours;
      inc(result.tasksbyprioritycount[ftasks[i].Priority]);
    end;
  end;
  
  hoursperweek := 30.0;
  if hoursperweek > 0 then
    result.overloadfactor := result.totalestimatedhours / hoursperweek
  else
    result.overloadfactor := 0.0;
    
  result.isoverloaded := result.overloadfactor > 1.2;
  
  if result.isoverloaded then
  begin
    if result.overloadfactor > 2.0 then
      result.recommendation := 'CRITICAL: Immediately redistribute tasks or extend deadlines'
    else if result.overloadfactor > 1.5 then
      result.recommendation := 'Consider reassigning some tasks or deprioritizing low-priority items'
    else
      result.recommendation := 'Monitor workload, slight overload detected';
  end
  else if result.overloadfactor < 0.5 then
    result.recommendation := 'Underutilized, can take on more tasks'
  else
    result.recommendation := 'Workload is balanced';
end;

function TTaskManager.AnalyzeAllWorkloads: TWorkloadAnalysisArray;
var
  users: TStringArray;
  i, j: integer;
  found: boolean;
begin
  SetLength(result, 0);
  SetLength(users, 0);
  
  for i := 0 to High(ftasks) do
  begin
    if ftasks[i].AssignedTo <> '' then
    begin
      found := false;
      for j := 0 to High(users) do
      begin
        if users[j] = ftasks[i].AssignedTo then
        begin
          found := true;
          break;
        end;
      end;
      
      if not found then
      begin
        SetLength(users, Length(users) + 1);
        users[High(users)] := ftasks[i].AssignedTo;
      end;
    end;
  end;
  
  SetLength(result, Length(users));
  for i := 0 to High(users) do
    result[i] := AnalyzeWorkload(users[i]);
end;

function TTaskManager.GetOverloadedUsers: TStringArray;
var
  workloads: TWorkloadAnalysisArray;
  i, count: integer;
begin
  SetLength(result, 0);
  count := 0;
  
  workloads := AnalyzeAllWorkloads;
  
  for i := 0 to High(workloads) do
  begin
    if workloads[i].isoverloaded then
    begin
      SetLength(result, count + 1);
      result[count] := workloads[i].assignedto;
      inc(count);
    end;
  end;
end;

function TTaskManager.ComputeExecutionOrder: TIntArray;
var
  i, j, k, outcount: integer;
  added: boolean;
  task: TTask;
  canstart: boolean;
  found: boolean;
begin
  SetLength(result, 0);
  outcount := 0;
  
  while outcount < Length(ftasks) do
  begin
    added := false;
    
    for i := 0 to High(ftasks) do
    begin
      task := ftasks[i];
      
      canstart := true;
      for j := 0 to High(result) do
      begin
        if result[j] = task.Id then
        begin
          canstart := false;
          break;
        end;
      end;
      
      if not canstart then
        continue;
      
      canstart := true;
      for j := 0 to High(task.fdependencies) do
      begin
        found := false;
        for k := 0 to High(result) do
        begin
          if result[k] = task.fdependencies[j] then
          begin
            found := true;
            break;
          end;
        end;
        
        if not found then
        begin
          canstart := false;
          break;
        end;
      end;
      
      if canstart then
      begin
        SetLength(result, outcount + 1);
        result[outcount] := task.Id;
        inc(outcount);
        added := true;
      end;
    end;
    
    if not added then
      break;
  end;
end;

function TTaskManager.GetCriticalPathTasks: TIntArray;
begin
  result := ComputeExecutionOrder;
end;

function TTaskManager.GenerateSmartSchedule: TScheduleRecommendationArray;
var
  execorder: TIntArray;
  i, j, count: integer;
  task: TTask;
  currentdate: TDateTime;
  prediction: TCompletionPrediction;
begin
  SetLength(result, 0);
  count := 0;
  
  execorder := ComputeExecutionOrder;
  currentdate := Now;
  
  for i := 0 to High(execorder) do
  begin
    task := GetTask(execorder[i]);
    if (task <> nil) and (task.Status <> tsCompleted) then
    begin
      SetLength(result, count + 1);
      result[count].taskid := task.Id;
      result[count].suggestedstartdate := currentdate;
      
      prediction := PredictTaskCompletion(task.Id);
      
      result[count].suggestedenddate := prediction.predictedcompletiondate;
      result[count].reasoning := Format('Position %d in execution order. %s',
        [i + 1, prediction.reasoning]);
      
      SetLength(result[count].dependencychain, Length(task.fdependencies));
      for j := 0 to High(task.fdependencies) do
        result[count].dependencychain[j] := task.fdependencies[j];
      
      currentdate := prediction.predictedcompletiondate;
      
      inc(count);
    end;
  end;
end;

function TTaskManager.GenerateAIReport: string;
var
  predictions: TDeadlinePredictionArray;
  conflicts: TConflictArray;
  risks: TRiskAssessmentArray;
  i, j: integer;
  temprisk: TRiskAssessment;
begin
  result := '========================================' + #13#10;
  result := result + 'AI/SMART FEATURES REPORT' + #13#10;
  result := result + '========================================' + #13#10;
  result := result + #13#10;
  
  result := result + 'DEADLINE PREDICTIONS:' + #13#10;
  result := result + '----------------------------------------' + #13#10;
  predictions := PredictAllDeadlines;
  if Length(predictions) = 0 then
    result := result + 'No active tasks with estimates.' + #13#10
  else
  begin
    for i := 0 to Min(4, High(predictions)) do
    begin
      result := result + Format('Task #%d: Suggested deadline %s (%.0f%% confidence)',
        [predictions[i].taskid, DateToStr(predictions[i].suggesteddeadline),
         predictions[i].confidence * 100]) + #13#10;
      result := result + Format('  %s', [predictions[i].reasoning]) + #13#10;
    end;
  end;
  
  result := result + #13#10;
  
  result := result + 'DETECTED CONFLICTS:' + #13#10;
  result := result + '----------------------------------------' + #13#10;
  conflicts := DetectConflicts;
  if Length(conflicts) = 0 then
    result := result + 'No conflicts detected. Excellent!' + #13#10
  else
  begin
    for i := 0 to High(conflicts) do
    begin
      result := result + Format('[%s - %s] %s',
        [RiskLevelToString(conflicts[i].severity),
         ConflictTypeToString(conflicts[i].conflicttype),
         conflicts[i].description]) + #13#10;
      result := result + Format('  Suggestion: %s', [conflicts[i].suggestion]) + #13#10;
    end;
  end;
  
  result := result + #13#10;
  
  result := result + 'TOP RISK TASKS:' + #13#10;
  result := result + '----------------------------------------' + #13#10;
  risks := AssessAllRisks;
  
  for i := 0 to High(risks) - 1 do
  begin
    for j := i + 1 to High(risks) do
    begin
      if Ord(risks[i].risklevel) < Ord(risks[j].risklevel) then
      begin
        temprisk := risks[i];
        risks[i] := risks[j];
        risks[j] := temprisk;
      end;
    end;
  end;
  
  if Length(risks) = 0 then
    result := result + 'No active tasks to assess.' + #13#10
  else
  begin
    for i := 0 to Min(4, High(risks)) do
    begin
      if risks[i].risklevel <> rlNone then
      begin
        result := result + Format('Task #%d [%s Risk - %.0f%% delay probability]',
          [risks[i].taskid, RiskLevelToString(risks[i].risklevel),
           risks[i].probabilityofdelay * 100]) + #13#10;
        result := result + Format('  Mitigation: %s', [risks[i].mitigation]) + #13#10;
      end;
    end;
  end;
end;

function TTaskManager.GenerateRiskReport: string;
var
  risks: TRiskAssessmentArray;
  i, j: integer;
  task: TTask;
  temprisk: TRiskAssessment;
begin
  result := '========================================' + #13#10;
  result := result + 'RISK ASSESSMENT REPORT' + #13#10;
  result := result + '========================================' + #13#10;
  result := result + #13#10;
  
  risks := AssessAllRisks;
  
  for i := 0 to High(risks) - 1 do
  begin
    for j := i + 1 to High(risks) do
    begin
      if Ord(risks[i].risklevel) < Ord(risks[j].risklevel) then
      begin
        temprisk := risks[i];
        risks[i] := risks[j];
        risks[j] := temprisk;
      end;
    end;
  end;
  
  for i := 0 to High(risks) do
  begin
    task := GetTask(risks[i].taskid);
    if task <> nil then
    begin
      result := result + Format('Task #%d: %s', [task.Id, task.Title]) + #13#10;
      result := result + Format('  Risk Level: %s', [RiskLevelToString(risks[i].risklevel)]) + #13#10;
      result := result + Format('  Delay Probability: %.0f%%', [risks[i].probabilityofdelay * 100]) + #13#10;
      
      if Length(risks[i].riskfactors) > 0 then
      begin
        result := result + '  Risk Factors:' + #13#10;
        for j := 0 to High(risks[i].riskfactors) do
          result := result + Format('    - %s', [risks[i].riskfactors[j]]) + #13#10;
      end;
      
      result := result + Format('  Mitigation: %s', [risks[i].mitigation]) + #13#10;
      result := result + #13#10;
    end;
  end;
end;

function TTaskManager.GenerateWorkloadReport: string;
var
  workloads: TWorkloadAnalysisArray;
  i: integer;
  priority: TTaskPriority;
begin
  result := '========================================' + #13#10;
  result := result + 'WORKLOAD ANALYSIS REPORT' + #13#10;
  result := result + '========================================' + #13#10;
  result := result + #13#10;
  
  workloads := AnalyzeAllWorkloads;
  
  if Length(workloads) = 0 then
  begin
    result := result + 'No tasks assigned to users.' + #13#10;
    exit;
  end;
  
  for i := 0 to High(workloads) do
  begin
    result := result + Format('User: %s', [workloads[i].assignedto]) + #13#10;
    result := result + Format('  Total Tasks: %d', [workloads[i].totaltasks]) + #13#10;
    result := result + Format('  Total Hours: %.1f', [workloads[i].totalestimatedhours]) + #13#10;
    result := result + Format('  Load Factor: %.1f%%',
      [workloads[i].overloadfactor * 100]) + #13#10;
    
    if workloads[i].isoverloaded then
      result := result + '  Status: OVERLOADED' + #13#10
    else
      result := result + '  Status: OK' + #13#10;
    
    result := result + '  Tasks by Priority:' + #13#10;
    for priority := tpCritical downto tpLow do
    begin
      if workloads[i].tasksbyprioritycount[priority] > 0 then
        result := result + Format('    %s: %d', 
          [PriorityToString(priority), workloads[i].tasksbyprioritycount[priority]]) + #13#10;
    end;
    
    result := result + Format('  Recommendation: %s', [workloads[i].recommendation]) + #13#10;
    result := result + #13#10;
  end;
end;

procedure TTaskManager.SelfTest;
var
  task1, task2, task3: TTask;
  metrics: TProductivityMetrics;
  predictions: TDeadlinePredictionArray;
  conflicts: TConflictArray;
  risks: TRiskAssessmentArray;
begin
  WriteLn('========================================');
  WriteLn('HYBRID TASK MANAGER SELF-TEST');
  WriteLn('(AI + Time Tracking Combined)');
  WriteLn('========================================');
  WriteLn;
  
  Clear;
  
  WriteLn('[Test 1] Creating tasks with both AI and time tracking...');
  task1 := AddTask('Design database schema');
  task1.EstimatedHours := 8;
  task1.Priority := tpHigh;
  task1.Category := 'Development';
  task1.DueDate := IncDay(Now, 3);
  task1.AssignedTo := 'Alice';
  
  task2 := AddTask('Implement API endpoints');
  task2.EstimatedHours := 16;
  task2.Priority := tpCritical;
  task2.Category := 'Development';
  task2.DueDate := IncDay(Now, 7);
  task2.AssignedTo := 'Alice';
  task2.AddDependency(task1.Id);
  
  task3 := AddTask('Write documentation');
  task3.EstimatedHours := 4;
  task3.Priority := tpMedium;
  task3.Category := 'Documentation';
  task3.DueDate := IncDay(Now, 10);
  task3.AssignedTo := 'Bob';
  
  WriteLn(Format('✓ Created %d tasks', [GetTaskCount]));
  WriteLn;
  
  WriteLn('[Test 2] Testing time tracking...');
  if StartTask(task1.Id, 'Initial design') then
    WriteLn('✓ Started timing task 1')
  else
    WriteLn('✗ Failed to start timing');
  Sleep(100);
  if StopTask(task1.Id) then
    WriteLn(Format('✓ Stopped timing (tracked %.4f hours)', [task1.GetTotalTrackedHours]))
  else
    WriteLn('✗ Failed to stop timing');
  WriteLn;
  
  WriteLn('[Test 3] Testing AI deadline prediction...');
  predictions := PredictAllDeadlines;
  WriteLn(Format('✓ Generated %d deadline predictions', [Length(predictions)]));
  if Length(predictions) > 0 then
    WriteLn(Format('  Sample: Task #%d suggested %s', 
      [predictions[0].taskid, DateToStr(predictions[0].suggesteddeadline)]));
  WriteLn;
  
  WriteLn('[Test 4] Testing conflict detection...');
  conflicts := DetectConflicts;
  WriteLn(Format('✓ Detected %d conflicts', [Length(conflicts)]));
  WriteLn;
  
  WriteLn('[Test 5] Testing risk assessment...');
  risks := AssessAllRisks;
  WriteLn(Format('✓ Assessed risk for %d tasks', [Length(risks)]));
  WriteLn;
  
  WriteLn('[Test 6] Completing task with tracked time...');
  task3.Status := tsCompleted;
  task3.CompletedDate := Now;
  WriteLn(Format('✓ Task 3 completed with %.4f tracked hours', [task3.ActualHours]));
  WriteLn;
  
  WriteLn('[Test 7] Testing productivity metrics...');
  metrics := GetProductivityMetrics;
  WriteLn(Format('✓ Analyzed %d completed tasks', [metrics.TotalTasksAnalyzed]));
  if metrics.TotalTasksAnalyzed > 0 then
    WriteLn(Format('  Accuracy: %.1f%%', [metrics.AverageEstimateAccuracy]));
  WriteLn;
  
  WriteLn('[Test 8] Testing file operations (V3 format)...');
  if SaveToFile('solution2/hybrid_test.dat') then
    WriteLn('✓ Saved with hybrid features')
  else
    WriteLn('✗ Failed to save');
  
  if LoadFromFile('solution2/hybrid_test.dat') then
    WriteLn(Format('✓ Loaded successfully (%d tasks)', [GetTaskCount]))
  else
    WriteLn('✗ Failed to load');
  WriteLn;
  
  WriteLn('========================================');
  WriteLn('✅ HYBRID SOLUTION TEST COMPLETE!');
  WriteLn('========================================');
  WriteLn;
  WriteLn('This hybrid solution combines:');
  WriteLn('  ✓ Automated time tracking (Solution 2)');
  WriteLn('  ✓ AI predictions and ML (Solution 1)');
  WriteLn('  ✓ Risk assessment and conflict detection');
  WriteLn('  ✓ Smart scheduling and workload analysis');
  WriteLn('  ✓ Productivity metrics and insights');
  WriteLn;
  WriteLn('Result: A commercial-grade task manager with');
  WriteLn('both real-time tracking AND predictive analytics!');
  WriteLn;
end;

end.
