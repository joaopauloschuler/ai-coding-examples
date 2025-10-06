
program TaskManager;
{$mode objfpc}
{$H+}

uses
  CThreads,
  SysUtils, Classes, DateUtils, Math, Process, BaseUnix;

type
  // Process state enumeration
  TProcessState = (psRunning, psSleeping, psDiskSleep, psZombie, psStopped, psTracingStop, psDead, psUnknown);
  
  // NEW: Security Risk Level
  TSecurityRiskLevel = (srlSafe, srlLowRisk, srlMediumRisk, srlHighRisk, srlCritical);
  
  // NEW: Security Threat Type
  TSecurityThreatType = (
    sttNone,
    sttRootPrivilege,
    sttSuidProcess,
    sttSuspiciousLocation,
    sttDeletedExecutable,
    sttExcessiveConnections,
    sttPrivilegedPort,
    sttRawSocket,
    sttSuspiciousName,
    sttOrphanProcess,
    sttCryptoMining,
    sttMemoryBomb,
    sttForkBomb,
    sttCapabilityElevated,
    sttListeningAllInterfaces
  );
  
  // NEW: Security Threat Set
  TSecurityThreats = set of TSecurityThreatType;
  
  // NEW: Process Capabilities
  TProcessCapabilities = record
    CapInheritable: string;
    CapPermitted: string;
    CapEffective: string;
    CapBounding: string;
    CapAmbient: string;
    HasElevatedCaps: Boolean;
  end;
  
  // NEW: Security Information
  TSecurityInfo = record
    UID: Integer;
    GID: Integer;
    EUID: Integer;  // Effective UID
    EGID: Integer;  // Effective GID
    IsRoot: Boolean;
    IsSUID: Boolean;
    IsSGID: Boolean;
    ExecutablePath: string;
    ExecutableDeleted: Boolean;
    SuspiciousLocation: Boolean;
    SuspiciousName: Boolean;
    Capabilities: TProcessCapabilities;
    ListeningPorts: array of Integer;
    HasPrivilegedPort: Boolean;
    HasRawSocket: Boolean;
    ListeningAllInterfaces: Boolean;
    Threats: TSecurityThreats;
    RiskScore: Integer;  // 0-100
    RiskLevel: TSecurityRiskLevel;
    ThreatDescription: string;
  end;
  
  // NEW: Security Event
  TSecurityEvent = record
    Timestamp: TDateTime;
    PID: Integer;
    ProcessName: string;
    EventType: TSecurityThreatType;
    Description: string;
    RiskLevel: TSecurityRiskLevel;
  end;
  
  TSecurityEventArray = array of TSecurityEvent;
  
  // NEW: System Security Statistics
  TSystemSecurityStats = record
    TotalProcesses: Integer;
    SafeProcesses: Integer;
    LowRiskProcesses: Integer;
    MediumRiskProcesses: Integer;
    HighRiskProcesses: Integer;
    CriticalRiskProcesses: Integer;
    RootProcesses: Integer;
    SuidProcesses: Integer;
    ProcessesWithPrivilegedPorts: Integer;
    ProcessesWithElevatedCaps: Integer;
    SuspiciousLocationProcesses: Integer;
    DeletedExecutableProcesses: Integer;
    OverallSecurityScore: Integer;  // 0-100 (100 = most secure)
    TotalThreatsDetected: Integer;
  end;
  
  // Detailed Memory Breakdown structure
  TMemoryBreakdown = record
    VmSize: Int64;
    VmRSS: Int64;
    VmPeak: Int64;
    VmHWM: Int64;
    RssAnon: Int64;
    RssFile: Int64;
    RssShmem: Int64;
    VmData: Int64;
    VmStk: Int64;
    VmExe: Int64;
    VmLib: Int64;
    VmPTE: Int64;
    VmSwap: Int64;
    VmLock: Int64;
    PrivateMemory: Int64;
    SharedMemory: Int64;
    TotalRSS: Int64;
    SwapUsage: Int64;
  end;
  
  // I/O Statistics structure
  TIOStats = record
    ReadChars: Int64;
    WriteChars: Int64;
    ReadSyscalls: Int64;
    WriteSyscalls: Int64;
    ReadBytes: Int64;
    WriteBytes: Int64;
    CancelledWriteBytes: Int64;
    ReadBytesPerSec: Double;
    WriteBytesPerSec: Double;
    TotalIOPerSec: Double;
  end;
  
  // Network Statistics structure
  TNetworkStats = record
    TCPConnections: Integer;
    UDPConnections: Integer;
    TCPListening: Integer;
    UnixSockets: Integer;
    TotalConnections: Integer;
  end;
  
  // Structure to hold process information
  TProcessInfo = record
    PID: Integer;
    ParentPID: Integer;
    Name: string;
    State: TProcessState;
    CPUPercent: Double;
    MemoryKB: Int64;
    VirtualMemoryKB: Int64;
    Threads: Integer;
    Priority: Integer;
    NiceValue: Integer;
    StartTime: TDateTime;
    CPUTime: Int64;
    UserName: string;
    CommandLine: string;
    IOStats: TIOStats;
    NetworkStats: TNetworkStats;
    MemBreakdown: TMemoryBreakdown;
    SecurityInfo: TSecurityInfo;  // NEW: Security information
  end;
  
  // Dynamic array types
  TProcessInfoArray = array of TProcessInfo;
  TStringArray = array of string;
  TIntegerArray = array of Integer;
  
  // System-wide memory breakdown statistics
  TSystemMemoryBreakdown = record
    TotalPrivateMemoryKB: Int64;
    TotalSharedMemoryKB: Int64;
    TotalSwapUsageKB: Int64;
    TotalVirtualMemoryKB: Int64;
    TotalResidentMemoryKB: Int64;
    ProcessesUsingSwap: Integer;
    LargestPrivateMemoryKB: Int64;
    LargestSharedMemoryKB: Int64;
    AveragePrivateMemoryKB: Int64;
    AverageSharedMemoryKB: Int64;
  end;
  
  // System statistics
  TSystemStats = record
    TotalProcesses: Integer;
    RunningProcesses: Integer;
    SleepingProcesses: Integer;
    ZombieProcesses: Integer;
    TotalMemoryKB: Int64;
    FreeMemoryKB: Int64;
    UsedMemoryKB: Int64;
    CachedMemoryKB: Int64;
    CPUCount: Integer;
    LoadAverage1: Double;
    LoadAverage5: Double;
    LoadAverage15: Double;
    UptimeSeconds: Int64;
    TotalDiskReadBytes: Int64;
    TotalDiskWriteBytes: Int64;
    TotalNetworkConnections: Integer;
    MemBreakdown: TSystemMemoryBreakdown;
    SecurityStats: TSystemSecurityStats;  // NEW: Security statistics
  end;
  
  // Sort criteria enumeration
  TSortCriteria = (scPID, scName, scCPU, scMemory, scThreads, 
                   scDiskRead, scDiskWrite, scTotalIO, scNetConnections,
                   scPrivateMemory, scSharedMemory, scSwapUsage, scVirtualMemory,
                   scSecurityRisk);  // NEW: Sort by security risk
  
  // Task Manager class
  TTaskManager = class
  private
    FProcessList: TProcessInfoArray;
    FLastCPUTime: array of Int64;
    FLastIOStats: array of TIOStats;
    FLastIOTime: array of TDateTime;
    FLastSystemTime: Int64;
    FSystemStats: TSystemStats;
    FSecurityEvents: TSecurityEventArray;  // NEW: Security event log
    FSecurityEventCount: Integer;
    
    function ReadFileToString(const FileName: string): string;
    function ReadFileToStrings(const FileName: string): TStringArray;
    function ParseStatFile(const StatFile: string; var Info: TProcessInfo): Boolean;
    function ParseStatusFile(const StatusFile: string; var Info: TProcessInfo): Boolean;
    function ParseCmdLineFile(const CmdLineFile: string): string;
    function ParseIOFile(const IOFile: string; var IOStats: TIOStats): Boolean;
    function GetNetworkStats(PID: Integer): TNetworkStats;
    function GetPIDList: TIntegerArray;
    function StateCharToEnum(StateChar: Char): TProcessState;
    function StateEnumToString(State: TProcessState): string;
    procedure CalculateCPUPercent(var Info: TProcessInfo; Index: Integer);
    procedure CalculateIORate(var Info: TProcessInfo; Index: Integer);
    procedure UpdateSystemStats;
    procedure CalculateMemoryBreakdownFields(var MemBreakdown: TMemoryBreakdown);
    procedure UpdateSystemMemoryBreakdown;
    function GetMemInfo: Boolean;
    function GetLoadAverage: Boolean;
    function GetUptime: Boolean;
    function ReadSymLink(const LinkPath: string): string;
    
    // NEW: Security analysis methods
    procedure AnalyzeProcessSecurity(var Info: TProcessInfo);
    function ParseSecurityInfo(PID: Integer; var SecInfo: TSecurityInfo): Boolean;
    function ParseCapabilities(const StatusFile: string; var Caps: TProcessCapabilities): Boolean;
    function GetExecutablePath(PID: Integer): string;
    function IsExecutableDeleted(const ExePath: string): Boolean;
    function IsSuspiciousLocation(const Path: string): Boolean;
    function IsSuspiciousName(const Name: string): Boolean;
    function GetListeningPorts(PID: Integer): TIntegerArray;
    function HasRawSocketAccess(PID: Integer): Boolean;
    function IsListeningAllInterfaces(PID: Integer): Boolean;
    function CalculateSecurityRiskScore(const SecInfo: TSecurityInfo; const Info: TProcessInfo): Integer;
    function DetermineRiskLevel(RiskScore: Integer): TSecurityRiskLevel;
    function GenerateThreatDescription(const SecInfo: TSecurityInfo): string;
    procedure LogSecurityEvent(const Info: TProcessInfo; ThreatType: TSecurityThreatType);
    procedure UpdateSecurityStatistics;
    function RiskLevelToString(Level: TSecurityRiskLevel): string;
    function ThreatTypeToString(ThreatType: TSecurityThreatType): string;
    function HexToCapabilities(const HexStr: string): string;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    // Main functions
    function RefreshProcessList: Integer;
    function GetProcessByPID(PID: Integer): TProcessInfo;
    function GetProcessesByName(const Name: string): TProcessInfoArray;
    function KillProcess(PID: Integer): Boolean;
    function KillProcessByName(const Name: string): Integer;
    
    // Sorting and filtering
    procedure SortProcesses(Criteria: TSortCriteria; Descending: Boolean);
    function FilterByMinMemory(MinMemoryKB: Int64): TProcessInfoArray;
    function FilterByMinCPU(MinCPUPercent: Double): TProcessInfoArray;
    function FilterByMinDiskIO(MinBytesPerSec: Double): TProcessInfoArray;
    function FilterByMinNetworkConnections(MinConnections: Integer): TProcessInfoArray;
    function FilterByMinPrivateMemory(MinPrivateKB: Int64): TProcessInfoArray;
    function FilterByMinSharedMemory(MinSharedKB: Int64): TProcessInfoArray;
    function FilterByMinSwap(MinSwapKB: Int64): TProcessInfoArray;
    function FilterByMemoryType(PrivatePercent: Double): TProcessInfoArray;
    
    function GetTopProcessesByMemory(Count: Integer): TProcessInfoArray;
    function GetTopProcessesByCPU(Count: Integer): TProcessInfoArray;
    function GetTopProcessesByDiskRead(Count: Integer): TProcessInfoArray;
    function GetTopProcessesByDiskWrite(Count: Integer): TProcessInfoArray;
    function GetTopProcessesByTotalIO(Count: Integer): TProcessInfoArray;
    function GetTopProcessesByNetworkConnections(Count: Integer): TProcessInfoArray;
    function GetTopProcessesByPrivateMemory(Count: Integer): TProcessInfoArray;
    function GetTopProcessesBySharedMemory(Count: Integer): TProcessInfoArray;
    function GetTopProcessesBySwap(Count: Integer): TProcessInfoArray;
    function GetTopProcessesByVirtualMemory(Count: Integer): TProcessInfoArray;
    
    // NEW: Security filtering and analysis
    function FilterByRiskLevel(MinLevel: TSecurityRiskLevel): TProcessInfoArray;
    function FilterByThreatType(ThreatType: TSecurityThreatType): TProcessInfoArray;
    function FilterRootProcesses: TProcessInfoArray;
    function FilterSuidProcesses: TProcessInfoArray;
    function FilterSuspiciousProcesses: TProcessInfoArray;
    function FilterDeletedExecutables: TProcessInfoArray;
    function FilterPrivilegedPorts: TProcessInfoArray;
    function FilterElevatedCapabilities: TProcessInfoArray;
    function GetTopRiskyProcesses(Count: Integer): TProcessInfoArray;
    function GetSecurityEvents: TSecurityEventArray;
    function GetSecurityEventsByRisk(MinLevel: TSecurityRiskLevel): TSecurityEventArray;
    function GetSystemSecurityStats: TSystemSecurityStats;
    
    // Statistics
    function GetSystemStats: TSystemStats;
    function GetTotalMemoryUsage: Int64;
    function GetProcessCount: Integer;
    function GetTotalDiskReadRate: Double;
    function GetTotalDiskWriteRate: Double;
    function GetTotalNetworkConnections: Integer;
    function GetSystemMemoryBreakdown: TSystemMemoryBreakdown;
    function GetTotalPrivateMemory: Int64;
    function GetTotalSharedMemory: Int64;
    function GetTotalSwapUsage: Int64;
    
    // Display functions
    function ProcessInfoToString(const Info: TProcessInfo): string;
    function ProcessInfoToStringWithIO(const Info: TProcessInfo): string;
    function MemoryBreakdownToString(const MemBreakdown: TMemoryBreakdown): string;
    function ProcessInfoToStringWithMemory(const Info: TProcessInfo): string;
    function ProcessMemoryDetailsToString(const Info: TProcessInfo): string;
    function SystemStatsToString: string;
    function FormatBytes(Bytes: Int64): string;
    function FormatBytesPerSec(BytesPerSec: Double): string;
    
    // NEW: Security display functions
    function SecurityInfoToString(const SecInfo: TSecurityInfo): string;
    function ProcessSecurityDetailsToString(const Info: TProcessInfo): string;
    function SecurityEventToString(const Event: TSecurityEvent): string;
    function SystemSecurityStatsToString: string;
    
    procedure PrintProcessList;
    procedure PrintProcessListWithIO;
    procedure PrintProcessListWithMemoryBreakdown;
    procedure PrintTopMemoryProcesses(Count: Integer);
    procedure PrintMemoryBreakdownStatistics;
    procedure PrintDetailedMemoryAnalysis(Count: Integer);
    procedure PrintSystemStats;
    procedure PrintIOStatistics;
    procedure PrintTopProcesses(Count: Integer);
    procedure PrintTopIOProcesses(Count: Integer);
    
    // NEW: Security printing functions
    procedure PrintSecurityAnalysis;
    procedure PrintSecurityThreats;
    procedure PrintRiskyProcesses(Count: Integer);
    procedure PrintSecurityEvents(Count: Integer);
    procedure PrintSecurityStatistics;
    procedure PrintDetailedSecurityAnalysis(Count: Integer);
    
    // NEW: Security export functions
    function ExportSecurityAnalysisToCSV(const FileName: string): Boolean;
    function ExportSecurityEventsToCSV(const FileName: string): Boolean;
    function ExportSecurityReportToText(const FileName: string): Boolean;
    
    function ExportMemoryBreakdownToCSV(const FileName: string): Boolean;
    function ExportMemoryBreakdownToText(const FileName: string): Boolean;
    
    // Self test
    procedure SelfTest;
    
    property ProcessList: TProcessInfoArray read FProcessList;
    property SystemStats: TSystemStats read FSystemStats;
    property SecurityEvents: TSecurityEventArray read FSecurityEvents;
  end;

{ TTaskManager Implementation }

constructor TTaskManager.Create;
begin
  inherited Create;
  SetLength(FProcessList, 0);
  SetLength(FLastCPUTime, 0);
  SetLength(FLastIOStats, 0);
  SetLength(FLastIOTime, 0);
  SetLength(FSecurityEvents, 0);
  FLastSystemTime := 0;
  FSecurityEventCount := 0;
end;

destructor TTaskManager.Destroy;
begin
  SetLength(FProcessList, 0);
  SetLength(FLastCPUTime, 0);
  SetLength(FLastIOStats, 0);
  SetLength(FLastIOTime, 0);
  SetLength(FSecurityEvents, 0);
  inherited Destroy;
end;

function TTaskManager.ReadSymLink(const LinkPath: string): string;
var
  Buffer: array[0..1023] of Char;
  Len: Integer;
begin
  Result := '';
  FillChar(Buffer, SizeOf(Buffer), 0);
  Len := FpReadLink(PChar(LinkPath), @Buffer[0], SizeOf(Buffer) - 1);
  if Len > 0 then
  begin
    Buffer[Len] := #0;
    Result := StrPas(Buffer);
  end;
end;

function TTaskManager.ReadFileToString(const FileName: string): string;
var
  F: TextFile;
  Line: string;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;
    
  try
    AssignFile(F, FileName);
    Reset(F);
    try
      while not Eof(F) do
      begin
        ReadLn(F, Line);
        Result := Result + Line + #10;
      end;
    finally
      CloseFile(F);
    end;
  except
    Result := '';
  end;
end;

function TTaskManager.ReadFileToStrings(const FileName: string): TStringArray;
var
  F: TextFile;
  Line: string;
  Count: Integer;
begin
  SetLength(Result, 0);
  if not FileExists(FileName) then
    Exit;
    
  Count := 0;
  try
    AssignFile(F, FileName);
    Reset(F);
    try
      while not Eof(F) do
      begin
        ReadLn(F, Line);
        SetLength(Result, Count + 1);
        Result[Count] := Line;
        Inc(Count);
      end;
    finally
      CloseFile(F);
    end;
  except
    SetLength(Result, 0);
  end;
end;

function TTaskManager.StateCharToEnum(StateChar: Char): TProcessState;
begin
  case StateChar of
    'R': Result := psRunning;
    'S': Result := psSleeping;
    'D': Result := psDiskSleep;
    'Z': Result := psZombie;
    'T': Result := psStopped;
    't': Result := psTracingStop;
    'X', 'x': Result := psDead;
  else
    Result := psUnknown;
  end;
end;

function TTaskManager.StateEnumToString(State: TProcessState): string;
begin
  case State of
    psRunning: Result := 'Running';
    psSleeping: Result := 'Sleeping';
    psDiskSleep: Result := 'Disk Sleep';
    psZombie: Result := 'Zombie';
    psStopped: Result := 'Stopped';
    psTracingStop: Result := 'Tracing Stop';
    psDead: Result := 'Dead';
  else
    Result := 'Unknown';
  end;
end;

function TTaskManager.GetPIDList: TIntegerArray;
var
  SearchRec: TSearchRec;
  Count: Integer;
  PID: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  if FindFirst('/proc/*', faDirectory, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if TryStrToInt(SearchRec.Name, PID) then
        begin
          SetLength(Result, Count + 1);
          Result[Count] := PID;
          Inc(Count);
        end;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

function TTaskManager.ParseStatFile(const StatFile: string; var Info: TProcessInfo): Boolean;
var
  Content: string;
  Parts: TStringArray;
  i: Integer;
  InParen: Boolean;
  CurrentPart: string;
  PartCount: Integer;
begin
  Result := False;
  Content := ReadFileToString(StatFile);
  if Content = '' then
    Exit;
    
  SetLength(Parts, 0);
  PartCount := 0;
  CurrentPart := '';
  InParen := False;
  
  for i := 1 to Length(Content) do
  begin
    if Content[i] = '(' then
      InParen := True
    else if Content[i] = ')' then
    begin
      InParen := False;
      SetLength(Parts, PartCount + 1);
      Parts[PartCount] := CurrentPart;
      Inc(PartCount);
      CurrentPart := '';
    end
    else if (Content[i] = ' ') and (not InParen) then
    begin
      if CurrentPart <> '' then
      begin
        SetLength(Parts, PartCount + 1);
        Parts[PartCount] := CurrentPart;
        Inc(PartCount);
        CurrentPart := '';
      end;
    end
    else if (Content[i] <> #10) and (Content[i] <> #13) then
      CurrentPart := CurrentPart + Content[i];
  end;
  
  if CurrentPart <> '' then
  begin
    SetLength(Parts, PartCount + 1);
    Parts[PartCount] := CurrentPart;
    Inc(PartCount);
  end;
  
  if PartCount < 20 then
    Exit;
    
  Info.PID := StrToIntDef(Parts[0], 0);
  Info.Name := Parts[1];
  if Length(Parts[2]) > 0 then
    Info.State := StateCharToEnum(Parts[2][1])
  else
    Info.State := psUnknown;
  Info.ParentPID := StrToIntDef(Parts[3], 0);
  
  if PartCount > 14 then
    Info.CPUTime := StrToInt64Def(Parts[13], 0) + StrToInt64Def(Parts[14], 0)
  else
    Info.CPUTime := 0;
    
  if PartCount > 19 then
    Info.Threads := StrToIntDef(Parts[19], 0)
  else
    Info.Threads := 1;
    
  if PartCount > 17 then
    Info.Priority := StrToIntDef(Parts[17], 0)
  else
    Info.Priority := 0;
    
  if PartCount > 18 then
    Info.NiceValue := StrToIntDef(Parts[18], 0)
  else
    Info.NiceValue := 0;
    
  if PartCount > 22 then
    Info.VirtualMemoryKB := StrToInt64Def(Parts[22], 0) div 1024
  else
    Info.VirtualMemoryKB := 0;
    
  Result := True;
end;

procedure TTaskManager.CalculateMemoryBreakdownFields(var MemBreakdown: TMemoryBreakdown);
begin
  MemBreakdown.PrivateMemory := MemBreakdown.RssAnon;
  MemBreakdown.SharedMemory := MemBreakdown.RssFile + MemBreakdown.RssShmem;
  MemBreakdown.TotalRSS := MemBreakdown.VmRSS;
  MemBreakdown.SwapUsage := MemBreakdown.VmSwap;
end;

function TTaskManager.ParseStatusFile(const StatusFile: string; var Info: TProcessInfo): Boolean;
var
  Lines: TStringArray;
  i: Integer;
  Line, Key, Value: string;
  ColonPos: Integer;
begin
  Result := False;
  Lines := ReadFileToStrings(StatusFile);
  
  FillChar(Info.MemBreakdown, SizeOf(Info.MemBreakdown), 0);
  
  for i := 0 to Length(Lines) - 1 do
  begin
    Line := Lines[i];
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
    begin
      Key := Trim(Copy(Line, 1, ColonPos - 1));
      Value := Trim(Copy(Line, ColonPos + 1, Length(Line)));
      Value := Trim(StringReplace(Value, ' kB', '', [rfReplaceAll]));
      
      if Key = 'VmSize' then
        Info.MemBreakdown.VmSize := StrToInt64Def(Value, 0)
      else if Key = 'VmRSS' then
      begin
        Info.MemBreakdown.VmRSS := StrToInt64Def(Value, 0);
        Info.MemoryKB := Info.MemBreakdown.VmRSS;
        Result := True;
      end
      else if Key = 'VmPeak' then
        Info.MemBreakdown.VmPeak := StrToInt64Def(Value, 0)
      else if Key = 'VmHWM' then
        Info.MemBreakdown.VmHWM := StrToInt64Def(Value, 0)
      else if Key = 'RssAnon' then
        Info.MemBreakdown.RssAnon := StrToInt64Def(Value, 0)
      else if Key = 'RssFile' then
        Info.MemBreakdown.RssFile := StrToInt64Def(Value, 0)
      else if Key = 'RssShmem' then
        Info.MemBreakdown.RssShmem := StrToInt64Def(Value, 0)
      else if Key = 'VmData' then
        Info.MemBreakdown.VmData := StrToInt64Def(Value, 0)
      else if Key = 'VmStk' then
        Info.MemBreakdown.VmStk := StrToInt64Def(Value, 0)
      else if Key = 'VmExe' then
        Info.MemBreakdown.VmExe := StrToInt64Def(Value, 0)
      else if Key = 'VmLib' then
        Info.MemBreakdown.VmLib := StrToInt64Def(Value, 0)
      else if Key = 'VmPTE' then
        Info.MemBreakdown.VmPTE := StrToInt64Def(Value, 0)
      else if Key = 'VmSwap' then
        Info.MemBreakdown.VmSwap := StrToInt64Def(Value, 0)
      else if Key = 'VmLck' then
        Info.MemBreakdown.VmLock := StrToInt64Def(Value, 0)
      else if Key = 'Uid' then
      begin
        Value := Trim(Copy(Value, 1, Pos(' ', Value + ' ') - 1));
        Info.UserName := Value;
      end;
    end;
  end;
  
  CalculateMemoryBreakdownFields(Info.MemBreakdown);
end;

function TTaskManager.ParseCmdLineFile(const CmdLineFile: string): string;
var
  F: File of Char;
  Ch: Char;
begin
  Result := '';
  if not FileExists(CmdLineFile) then
    Exit;
    
  try
    AssignFile(F, CmdLineFile);
    Reset(F);
    try
      while not Eof(F) do
      begin
        Read(F, Ch);
        if Ch = #0 then
          Ch := ' ';
        Result := Result + Ch;
      end;
    finally
      CloseFile(F);
    end;
  except
    Result := '';
  end;
  
  Result := Trim(Result);
end;

function TTaskManager.ParseIOFile(const IOFile: string; var IOStats: TIOStats): Boolean;
var
  Lines: TStringArray;
  i: Integer;
  Line, Key, Value: string;
  ColonPos: Integer;
begin
  Result := False;
  
  FillChar(IOStats, SizeOf(IOStats), 0);
  
  if not FileExists(IOFile) then
    Exit;
    
  Lines := ReadFileToStrings(IOFile);
  
  for i := 0 to Length(Lines) - 1 do
  begin
    Line := Lines[i];
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
    begin
      Key := Trim(Copy(Line, 1, ColonPos - 1));
      Value := Trim(Copy(Line, ColonPos + 1, Length(Line)));
      
      if Key = 'rchar' then
        IOStats.ReadChars := StrToInt64Def(Value, 0)
      else if Key = 'wchar' then
        IOStats.WriteChars := StrToInt64Def(Value, 0)
      else if Key = 'syscr' then
        IOStats.ReadSyscalls := StrToInt64Def(Value, 0)
      else if Key = 'syscw' then
        IOStats.WriteSyscalls := StrToInt64Def(Value, 0)
      else if Key = 'read_bytes' then
      begin
        IOStats.ReadBytes := StrToInt64Def(Value, 0);
        Result := True;
      end
      else if Key = 'write_bytes' then
      begin
        IOStats.WriteBytes := StrToInt64Def(Value, 0);
        Result := True;
      end
      else if Key = 'cancelled_write_bytes' then
        IOStats.CancelledWriteBytes := StrToInt64Def(Value, 0);
    end;
  end;
end;

function TTaskManager.GetNetworkStats(PID: Integer): TNetworkStats;
var
  FDDir: string;
  SearchRec: TSearchRec;
  LinkTarget: string;
  SocketCount: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  SocketCount := 0;
  
  FDDir := '/proc/' + IntToStr(PID) + '/fd';
  
  if not DirectoryExists(FDDir) then
    Exit;
    
  if FindFirst(FDDir + '/*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        try
          LinkTarget := ReadSymLink(FDDir + '/' + SearchRec.Name);
          
          if Pos('socket:', LinkTarget) > 0 then
            Inc(SocketCount);
        except
        end;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
  
  Result.TotalConnections := SocketCount;
  Result.TCPConnections := SocketCount;
end;

procedure TTaskManager.CalculateCPUPercent(var Info: TProcessInfo; Index: Integer);
var
  DeltaCPU, DeltaTime: Int64;
begin
  Info.CPUPercent := 0.0;
  
  if (Index >= 0) and (Index < Length(FLastCPUTime)) then
  begin
    DeltaCPU := Info.CPUTime - FLastCPUTime[Index];
    DeltaTime := FLastSystemTime;
    
    if DeltaTime > 0 then
      Info.CPUPercent := (DeltaCPU * 100.0) / DeltaTime;
  end;
  
  if Index >= 0 then
  begin
    if Index >= Length(FLastCPUTime) then
      SetLength(FLastCPUTime, Index + 1);
    FLastCPUTime[Index] := Info.CPUTime;
  end;
end;

procedure TTaskManager.CalculateIORate(var Info: TProcessInfo; Index: Integer);
var
  DeltaRead, DeltaWrite: Int64;
  DeltaTime: Double;
  CurrentTime: TDateTime;
begin
  Info.IOStats.ReadBytesPerSec := 0.0;
  Info.IOStats.WriteBytesPerSec := 0.0;
  Info.IOStats.TotalIOPerSec := 0.0;
  
  CurrentTime := Now;
  
  if (Index >= 0) and (Index < Length(FLastIOStats)) and (Index < Length(FLastIOTime)) then
  begin
    DeltaTime := (CurrentTime - FLastIOTime[Index]) * 86400.0;
    
    if DeltaTime > 0.001 then
    begin
      DeltaRead := Info.IOStats.ReadBytes - FLastIOStats[Index].ReadBytes;
      DeltaWrite := Info.IOStats.WriteBytes - FLastIOStats[Index].WriteBytes;
      
      if DeltaRead < 0 then DeltaRead := 0;
      if DeltaWrite < 0 then DeltaWrite := 0;
      
      Info.IOStats.ReadBytesPerSec := DeltaRead / DeltaTime;
      Info.IOStats.WriteBytesPerSec := DeltaWrite / DeltaTime;
      Info.IOStats.TotalIOPerSec := Info.IOStats.ReadBytesPerSec + Info.IOStats.WriteBytesPerSec;
    end;
  end;
  
  if Index >= 0 then
  begin
    if Index >= Length(FLastIOStats) then
      SetLength(FLastIOStats, Index + 1);
    if Index >= Length(FLastIOTime) then
      SetLength(FLastIOTime, Index + 1);
      
    FLastIOStats[Index] := Info.IOStats;
    FLastIOTime[Index] := CurrentTime;
  end;
end;

{ Security Analysis Implementation }

function TTaskManager.HexToCapabilities(const HexStr: string): string;
var
  CapValue: Int64;
  Caps: TStringList;
begin
  Result := '';
  if HexStr = '' then
    Exit;
    
  CapValue := StrToInt64Def('$' + HexStr, 0);
  
  if CapValue = 0 then
  begin
    Result := 'none';
    Exit;
  end;
  
  Caps := TStringList.Create;
  try
    if (CapValue and (1 shl 0)) <> 0 then Caps.Add('CHOWN');
    if (CapValue and (1 shl 1)) <> 0 then Caps.Add('DAC_OVERRIDE');
    if (CapValue and (1 shl 2)) <> 0 then Caps.Add('DAC_READ_SEARCH');
    if (CapValue and (1 shl 3)) <> 0 then Caps.Add('FOWNER');
    if (CapValue and (1 shl 4)) <> 0 then Caps.Add('FSETID');
    if (CapValue and (1 shl 5)) <> 0 then Caps.Add('KILL');
    if (CapValue and (1 shl 6)) <> 0 then Caps.Add('SETGID');
    if (CapValue and (1 shl 7)) <> 0 then Caps.Add('SETUID');
    if (CapValue and (1 shl 8)) <> 0 then Caps.Add('SETPCAP');
    if (CapValue and (1 shl 9)) <> 0 then Caps.Add('LINUX_IMMUTABLE');
    if (CapValue and (1 shl 10)) <> 0 then Caps.Add('NET_BIND_SERVICE');
    if (CapValue and (1 shl 11)) <> 0 then Caps.Add('NET_BROADCAST');
    if (CapValue and (1 shl 12)) <> 0 then Caps.Add('NET_ADMIN');
    if (CapValue and (1 shl 13)) <> 0 then Caps.Add('NET_RAW');
    if (CapValue and (1 shl 14)) <> 0 then Caps.Add('IPC_LOCK');
    if (CapValue and (1 shl 15)) <> 0 then Caps.Add('IPC_OWNER');
    if (CapValue and (1 shl 16)) <> 0 then Caps.Add('SYS_MODULE');
    if (CapValue and (1 shl 17)) <> 0 then Caps.Add('SYS_RAWIO');
    if (CapValue and (1 shl 18)) <> 0 then Caps.Add('SYS_CHROOT');
    if (CapValue and (1 shl 19)) <> 0 then Caps.Add('SYS_PTRACE');
    if (CapValue and (1 shl 20)) <> 0 then Caps.Add('SYS_PACCT');
    if (CapValue and (1 shl 21)) <> 0 then Caps.Add('SYS_ADMIN');
    if (CapValue and (1 shl 22)) <> 0 then Caps.Add('SYS_BOOT');
    if (CapValue and (1 shl 23)) <> 0 then Caps.Add('SYS_NICE');
    if (CapValue and (1 shl 24)) <> 0 then Caps.Add('SYS_RESOURCE');
    if (CapValue and (1 shl 25)) <> 0 then Caps.Add('SYS_TIME');
    
    if Caps.Count > 0 then
      Result := Caps.CommaText
    else
      Result := 'none';
  finally
    Caps.Free;
  end;
end;

function TTaskManager.ParseCapabilities(const StatusFile: string; var Caps: TProcessCapabilities): Boolean;
var
  Lines: TStringArray;
  i: Integer;
  Line, Key, Value: string;
  ColonPos: Integer;
begin
  Result := False;
  Lines := ReadFileToStrings(StatusFile);
  
  FillChar(Caps, SizeOf(Caps), 0);
  Caps.HasElevatedCaps := False;
  
  for i := 0 to Length(Lines) - 1 do
  begin
    Line := Lines[i];
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
    begin
      Key := Trim(Copy(Line, 1, ColonPos - 1));
      Value := Trim(Copy(Line, ColonPos + 1, Length(Line)));
      
      if Key = 'CapInh' then
      begin
        Caps.CapInheritable := Value;
        Result := True;
      end
      else if Key = 'CapPrm' then
      begin
        Caps.CapPermitted := Value;
        if Value <> '0000000000000000' then
          Caps.HasElevatedCaps := True;
      end
      else if Key = 'CapEff' then
      begin
        Caps.CapEffective := Value;
        if Value <> '0000000000000000' then
          Caps.HasElevatedCaps := True;
      end
      else if Key = 'CapBnd' then
        Caps.CapBounding := Value
      else if Key = 'CapAmb' then
        Caps.CapAmbient := Value;
    end;
  end;
end;

function TTaskManager.GetExecutablePath(PID: Integer): string;
begin
  Result := ReadSymLink('/proc/' + IntToStr(PID) + '/exe');
end;

function TTaskManager.IsExecutableDeleted(const ExePath: string): Boolean;
begin
  Result := (Pos('(deleted)', ExePath) > 0);
end;

function TTaskManager.IsSuspiciousLocation(const Path: string): Boolean;
var
  LowerPath: string;
begin
  LowerPath := LowerCase(Path);
  Result := (Pos('/tmp/', LowerPath) > 0) or
            (Pos('/dev/shm/', LowerPath) > 0) or
            (Pos('/var/tmp/', LowerPath) > 0) or
            (Pos('/.', LowerPath) > 0);
end;

function TTaskManager.IsSuspiciousName(const Name: string): Boolean;
var
  LowerName: string;
begin
  LowerName := LowerCase(Name);
  Result := (Length(Name) < 2) or
            (Pos('[', Name) = 1) or
            (Pos('...', LowerName) > 0) or
            (Pos('..', Name) = 1);
end;

function TTaskManager.GetListeningPorts(PID: Integer): TIntegerArray;
var
  NetTCP, NetTCP6: TStringArray;
  i, Count: Integer;
  Line, LocalAddr, State: string;
  Parts: TStringArray;
  PortStr: string;
  Port, ColonPos: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  NetTCP := ReadFileToStrings('/proc/net/tcp');
  NetTCP6 := ReadFileToStrings('/proc/net/tcp6');
  
  for i := 1 to Length(NetTCP) - 1 do
  begin
    Line := NetTCP[i];
    if Trim(Line) = '' then
      Continue;
      
    SetLength(Parts, 0);
    Parts := Line.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
    
    if Length(Parts) >= 4 then
    begin
      LocalAddr := Parts[1];
      State := Parts[3];
      
      if State = '0A' then
      begin
        ColonPos := Pos(':', LocalAddr);
        if ColonPos > 0 then
        begin
          PortStr := Copy(LocalAddr, ColonPos + 1, Length(LocalAddr));
          Port := StrToIntDef('$' + PortStr, 0);
          if Port > 0 then
          begin
            SetLength(Result, Count + 1);
            Result[Count] := Port;
            Inc(Count);
          end;
        end;
      end;
    end;
  end;
end;

function TTaskManager.HasRawSocketAccess(PID: Integer): Boolean;
var
  StatusFile: string;
  Caps: TProcessCapabilities;
begin
  Result := False;
  StatusFile := '/proc/' + IntToStr(PID) + '/status';
  if ParseCapabilities(StatusFile, Caps) then
  begin
    Result := (Pos('NET_RAW', HexToCapabilities(Caps.CapEffective)) > 0) or
              (Pos('NET_ADMIN', HexToCapabilities(Caps.CapEffective)) > 0);
  end;
end;

function TTaskManager.IsListeningAllInterfaces(PID: Integer): Boolean;
var
  NetTCP: TStringArray;
  i: Integer;
  Line, LocalAddr: string;
  Parts: TStringArray;
begin
  Result := False;
  
  NetTCP := ReadFileToStrings('/proc/net/tcp');
  
  for i := 1 to Length(NetTCP) - 1 do
  begin
    Line := NetTCP[i];
    if Trim(Line) = '' then
      Continue;
      
    Parts := Line.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
    
    if Length(Parts) >= 2 then
    begin
      LocalAddr := Parts[1];
      if Pos('00000000:', LocalAddr) = 1 then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TTaskManager.ParseSecurityInfo(PID: Integer; var SecInfo: TSecurityInfo): Boolean;
var
  StatusFile: string;
  Lines: TStringArray;
  i: Integer;
  Line, Key, Value: string;
  ColonPos: Integer;
  UIDValues: TStringArray;
begin
  Result := False;
  StatusFile := '/proc/' + IntToStr(PID) + '/status';
  
  if not FileExists(StatusFile) then
    Exit;
    
  Lines := ReadFileToStrings(StatusFile);
  
  FillChar(SecInfo, SizeOf(SecInfo), 0);
  SecInfo.Threats := [];
  
  for i := 0 to Length(Lines) - 1 do
  begin
    Line := Lines[i];
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
    begin
      Key := Trim(Copy(Line, 1, ColonPos - 1));
      Value := Trim(Copy(Line, ColonPos + 1, Length(Line)));
      
      if Key = 'Uid' then
      begin
        UIDValues := Value.Split([#9, ' '], TStringSplitOptions.ExcludeEmpty);
        if Length(UIDValues) >= 2 then
        begin
          SecInfo.UID := StrToIntDef(UIDValues[0], -1);
          SecInfo.EUID := StrToIntDef(UIDValues[1], -1);
          SecInfo.IsRoot := (SecInfo.UID = 0) or (SecInfo.EUID = 0);
          SecInfo.IsSUID := (SecInfo.UID <> SecInfo.EUID);
          Result := True;
        end;
      end
      else if Key = 'Gid' then
      begin
        UIDValues := Value.Split([#9, ' '], TStringSplitOptions.ExcludeEmpty);
        if Length(UIDValues) >= 2 then
        begin
          SecInfo.GID := StrToIntDef(UIDValues[0], -1);
          SecInfo.EGID := StrToIntDef(UIDValues[1], -1);
          SecInfo.IsSGID := (SecInfo.GID <> SecInfo.EGID);
        end;
      end;
    end;
  end;
  
  ParseCapabilities(StatusFile, SecInfo.Capabilities);
  
  SecInfo.ExecutablePath := GetExecutablePath(PID);
  SecInfo.ExecutableDeleted := IsExecutableDeleted(SecInfo.ExecutablePath);
  SecInfo.SuspiciousLocation := IsSuspiciousLocation(SecInfo.ExecutablePath);
  
  SecInfo.ListeningPorts := GetListeningPorts(PID);
  SecInfo.HasPrivilegedPort := False;
  for i := 0 to Length(SecInfo.ListeningPorts) - 1 do
  begin
    if SecInfo.ListeningPorts[i] < 1024 then
    begin
      SecInfo.HasPrivilegedPort := True;
      Break;
    end;
  end;
  
  SecInfo.HasRawSocket := HasRawSocketAccess(PID);
  SecInfo.ListeningAllInterfaces := IsListeningAllInterfaces(PID);
end;

function TTaskManager.CalculateSecurityRiskScore(const SecInfo: TSecurityInfo; const Info: TProcessInfo): Integer;
var
  Score: Integer;
begin
  Score := 0;
  
  if SecInfo.IsRoot then
    Score := Score + 20;
    
  if SecInfo.IsSUID then
    Score := Score + 25;
    
  if SecInfo.IsSGID then
    Score := Score + 15;
    
  if SecInfo.ExecutableDeleted then
    Score := Score + 30;
    
  if SecInfo.SuspiciousLocation then
    Score := Score + 25;
    
  if SecInfo.SuspiciousName then
    Score := Score + 20;
    
  if SecInfo.HasPrivilegedPort then
    Score := Score + 15;
    
  if SecInfo.HasRawSocket then
    Score := Score + 20;
    
  if SecInfo.ListeningAllInterfaces then
    Score := Score + 10;
    
  if SecInfo.Capabilities.HasElevatedCaps then
    Score := Score + 15;
    
  if Info.NetworkStats.TotalConnections > 50 then
    Score := Score + 15;
    
  if (Info.CPUPercent > 80.0) and (Info.NetworkStats.TotalConnections > 10) then
    Score := Score + 20;
    
  if Info.ParentPID = 1 then
    Score := Score - 10;
    
  if Score < 0 then
    Score := 0;
  if Score > 100 then
    Score := 100;
    
  Result := Score;
end;

function TTaskManager.DetermineRiskLevel(RiskScore: Integer): TSecurityRiskLevel;
begin
  if RiskScore >= 75 then
    Result := srlCritical
  else if RiskScore >= 50 then
    Result := srlHighRisk
  else if RiskScore >= 30 then
    Result := srlMediumRisk
  else if RiskScore >= 10 then
    Result := srlLowRisk
  else
    Result := srlSafe;
end;

function TTaskManager.GenerateThreatDescription(const SecInfo: TSecurityInfo): string;
var
  Threats: TStringList;
begin
  Threats := TStringList.Create;
  try
    if SecInfo.IsRoot then
      Threats.Add('Running as root');
    if SecInfo.IsSUID then
      Threats.Add('SUID process');
    if SecInfo.IsSGID then
      Threats.Add('SGID process');
    if SecInfo.ExecutableDeleted then
      Threats.Add('Deleted executable');
    if SecInfo.SuspiciousLocation then
      Threats.Add('Suspicious location');
    if SecInfo.SuspiciousName then
      Threats.Add('Suspicious name');
    if SecInfo.HasPrivilegedPort then
      Threats.Add('Privileged port');
    if SecInfo.HasRawSocket then
      Threats.Add('Raw socket access');
    if SecInfo.ListeningAllInterfaces then
      Threats.Add('Listening on all interfaces');
    if SecInfo.Capabilities.HasElevatedCaps then
      Threats.Add('Elevated capabilities');
      
    if Threats.Count > 0 then
      Result := Threats.CommaText
    else
      Result := 'No threats detected';
  finally
    Threats.Free;
  end;
end;

procedure TTaskManager.LogSecurityEvent(const Info: TProcessInfo; ThreatType: TSecurityThreatType);
var
  Event: TSecurityEvent;
begin
  Event.Timestamp := Now;
  Event.PID := Info.PID;
  Event.ProcessName := Info.Name;
  Event.EventType := ThreatType;
  Event.RiskLevel := Info.SecurityInfo.RiskLevel;
  Event.Description := GenerateThreatDescription(Info.SecurityInfo);
  
  if FSecurityEventCount >= Length(FSecurityEvents) then
    SetLength(FSecurityEvents, FSecurityEventCount + 100);
    
  FSecurityEvents[FSecurityEventCount] := Event;
  Inc(FSecurityEventCount);
end;

procedure TTaskManager.AnalyzeProcessSecurity(var Info: TProcessInfo);
begin
  FillChar(Info.SecurityInfo, SizeOf(Info.SecurityInfo), 0);
  
  if not ParseSecurityInfo(Info.PID, Info.SecurityInfo) then
    Exit;
    
  Info.SecurityInfo.SuspiciousName := IsSuspiciousName(Info.Name);
  
  Info.SecurityInfo.Threats := [];
  
  if Info.SecurityInfo.IsRoot then
  begin
    Include(Info.SecurityInfo.Threats, sttRootPrivilege);
    LogSecurityEvent(Info, sttRootPrivilege);
  end;
  
  if Info.SecurityInfo.IsSUID then
  begin
    Include(Info.SecurityInfo.Threats, sttSuidProcess);
    LogSecurityEvent(Info, sttSuidProcess);
  end;
  
  if Info.SecurityInfo.ExecutableDeleted then
  begin
    Include(Info.SecurityInfo.Threats, sttDeletedExecutable);
    LogSecurityEvent(Info, sttDeletedExecutable);
  end;
  
  if Info.SecurityInfo.SuspiciousLocation then
  begin
    Include(Info.SecurityInfo.Threats, sttSuspiciousLocation);
    LogSecurityEvent(Info, sttSuspiciousLocation);
  end;
  
  if Info.SecurityInfo.SuspiciousName then
  begin
    Include(Info.SecurityInfo.Threats, sttSuspiciousName);
    LogSecurityEvent(Info, sttSuspiciousName);
  end;
  
  if Info.SecurityInfo.HasPrivilegedPort then
  begin
    Include(Info.SecurityInfo.Threats, sttPrivilegedPort);
    LogSecurityEvent(Info, sttPrivilegedPort);
  end;
  
  if Info.SecurityInfo.HasRawSocket then
  begin
    Include(Info.SecurityInfo.Threats, sttRawSocket);
    LogSecurityEvent(Info, sttRawSocket);
  end;
  
  if Info.SecurityInfo.ListeningAllInterfaces then
  begin
    Include(Info.SecurityInfo.Threats, sttListeningAllInterfaces);
    LogSecurityEvent(Info, sttListeningAllInterfaces);
  end;
  
  if Info.SecurityInfo.Capabilities.HasElevatedCaps then
  begin
    Include(Info.SecurityInfo.Threats, sttCapabilityElevated);
    LogSecurityEvent(Info, sttCapabilityElevated);
  end;
  
  if Info.NetworkStats.TotalConnections > 100 then
  begin
    Include(Info.SecurityInfo.Threats, sttExcessiveConnections);
    LogSecurityEvent(Info, sttExcessiveConnections);
  end;
  
  if (Info.CPUPercent > 90.0) and (Info.Threads > 4) then
  begin
    Include(Info.SecurityInfo.Threats, sttCryptoMining);
    LogSecurityEvent(Info, sttCryptoMining);
  end;
  
  Info.SecurityInfo.RiskScore := CalculateSecurityRiskScore(Info.SecurityInfo, Info);
  Info.SecurityInfo.RiskLevel := DetermineRiskLevel(Info.SecurityInfo.RiskScore);
  Info.SecurityInfo.ThreatDescription := GenerateThreatDescription(Info.SecurityInfo);
end;

function TTaskManager.RiskLevelToString(Level: TSecurityRiskLevel): string;
begin
  case Level of
    srlSafe: Result := 'Safe';
    srlLowRisk: Result := 'Low Risk';
    srlMediumRisk: Result := 'Medium Risk';
    srlHighRisk: Result := 'High Risk';
    srlCritical: Result := 'CRITICAL';
  else
    Result := 'Unknown';
  end;
end;

function TTaskManager.ThreatTypeToString(ThreatType: TSecurityThreatType): string;
begin
  case ThreatType of
    sttNone: Result := 'None';
    sttRootPrivilege: Result := 'Root Privilege';
    sttSuidProcess: Result := 'SUID Process';
    sttSuspiciousLocation: Result := 'Suspicious Location';
    sttDeletedExecutable: Result := 'Deleted Executable';
    sttExcessiveConnections: Result := 'Excessive Connections';
    sttPrivilegedPort: Result := 'Privileged Port';
    sttRawSocket: Result := 'Raw Socket';
    sttSuspiciousName: Result := 'Suspicious Name';
    sttOrphanProcess: Result := 'Orphan Process';
    sttCryptoMining: Result := 'Possible Crypto Mining';
    sttMemoryBomb: Result := 'Memory Bomb';
    sttForkBomb: Result := 'Fork Bomb';
    sttCapabilityElevated: Result := 'Elevated Capabilities';
    sttListeningAllInterfaces: Result := 'Listening All Interfaces';
  else
    Result := 'Unknown';
  end;
end;

procedure TTaskManager.UpdateSecurityStatistics;
var
  i: Integer;
  TotalRisk: Int64;
begin
  FillChar(FSystemStats.SecurityStats, SizeOf(FSystemStats.SecurityStats), 0);
  
  FSystemStats.SecurityStats.TotalProcesses := Length(FProcessList);
  TotalRisk := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    case FProcessList[i].SecurityInfo.RiskLevel of
      srlSafe: Inc(FSystemStats.SecurityStats.SafeProcesses);
      srlLowRisk: Inc(FSystemStats.SecurityStats.LowRiskProcesses);
      srlMediumRisk: Inc(FSystemStats.SecurityStats.MediumRiskProcesses);
      srlHighRisk: Inc(FSystemStats.SecurityStats.HighRiskProcesses);
      srlCritical: Inc(FSystemStats.SecurityStats.CriticalRiskProcesses);
    end;
    
    if FProcessList[i].SecurityInfo.IsRoot then
      Inc(FSystemStats.SecurityStats.RootProcesses);
      
    if FProcessList[i].SecurityInfo.IsSUID then
      Inc(FSystemStats.SecurityStats.SuidProcesses);
      
    if FProcessList[i].SecurityInfo.HasPrivilegedPort then
      Inc(FSystemStats.SecurityStats.ProcessesWithPrivilegedPorts);
      
    if FProcessList[i].SecurityInfo.Capabilities.HasElevatedCaps then
      Inc(FSystemStats.SecurityStats.ProcessesWithElevatedCaps);
      
    if FProcessList[i].SecurityInfo.SuspiciousLocation then
      Inc(FSystemStats.SecurityStats.SuspiciousLocationProcesses);
      
    if FProcessList[i].SecurityInfo.ExecutableDeleted then
      Inc(FSystemStats.SecurityStats.DeletedExecutableProcesses);
      
    FSystemStats.SecurityStats.TotalThreatsDetected := FSystemStats.SecurityStats.TotalThreatsDetected + 
      Integer(FProcessList[i].SecurityInfo.Threats <> []);
      
    TotalRisk := TotalRisk + FProcessList[i].SecurityInfo.RiskScore;
  end;
  
  if Length(FProcessList) > 0 then
    FSystemStats.SecurityStats.OverallSecurityScore := 100 - (TotalRisk div Length(FProcessList))
  else
    FSystemStats.SecurityStats.OverallSecurityScore := 100;
end;

function TTaskManager.RefreshProcessList: Integer;
var
  PIDs: TIntegerArray;
  i: Integer;
  Info: TProcessInfo;
  ProcDir: string;
begin
  PIDs := GetPIDList;
  SetLength(FProcessList, Length(PIDs));
  Result := 0;
  
  for i := 0 to Length(PIDs) - 1 do
  begin
    ProcDir := '/proc/' + IntToStr(PIDs[i]);
    
    FillChar(Info, SizeOf(Info), 0);
    Info.PID := PIDs[i];
    Info.State := psUnknown;
    
    if ParseStatFile(ProcDir + '/stat', Info) then
    begin
      ParseStatusFile(ProcDir + '/status', Info);
      Info.CommandLine := ParseCmdLineFile(ProcDir + '/cmdline');
      if Info.CommandLine = '' then
        Info.CommandLine := Info.Name;
        
      CalculateCPUPercent(Info, i);
      
      if ParseIOFile(ProcDir + '/io', Info.IOStats) then
        CalculateIORate(Info, i);
        
      Info.NetworkStats := GetNetworkStats(PIDs[i]);
      
      AnalyzeProcessSecurity(Info);
      
      FProcessList[Result] := Info;
      Inc(Result);
    end;
  end;
  
  SetLength(FProcessList, Result);
  UpdateSystemStats;
  UpdateSystemMemoryBreakdown;
  UpdateSecurityStatistics;
end;

function TTaskManager.GetProcessByPID(PID: Integer): TProcessInfo;
var
  i: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].PID = PID then
    begin
      Result := FProcessList[i];
      Exit;
    end;
  end;
end;

function TTaskManager.GetProcessesByName(const Name: string): TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if Pos(LowerCase(Name), LowerCase(FProcessList[i].Name)) > 0 then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.KillProcess(PID: Integer): Boolean;
var
  Proc: TProcess;
begin
  Result := False;
  try
    Proc := TProcess.Create(nil);
    try
      Proc.Executable := 'kill';
      Proc.Parameters.Add('-9');
      Proc.Parameters.Add(IntToStr(PID));
      Proc.Options := [poWaitOnExit, poNoConsole];
      Proc.Execute;
      Result := Proc.ExitStatus = 0;
    finally
      Proc.Free;
    end;
  except
    Result := False;
  end;
end;

function TTaskManager.KillProcessByName(const Name: string): Integer;
var
  Processes: TProcessInfoArray;
  i: Integer;
begin
  Result := 0;
  Processes := GetProcessesByName(Name);
  
  for i := 0 to Length(Processes) - 1 do
  begin
    if KillProcess(Processes[i].PID) then
      Inc(Result);
  end;
end;

procedure TTaskManager.SortProcesses(Criteria: TSortCriteria; Descending: Boolean);
var
  i, j: Integer;
  Temp: TProcessInfo;
  ShouldSwap: Boolean;
begin
  for i := 0 to Length(FProcessList) - 2 do
  begin
    for j := i + 1 to Length(FProcessList) - 1 do
    begin
      ShouldSwap := False;
      
      case Criteria of
        scPID: ShouldSwap := FProcessList[i].PID > FProcessList[j].PID;
        scName: ShouldSwap := FProcessList[i].Name > FProcessList[j].Name;
        scCPU: ShouldSwap := FProcessList[i].CPUPercent > FProcessList[j].CPUPercent;
        scMemory: ShouldSwap := FProcessList[i].MemoryKB > FProcessList[j].MemoryKB;
        scThreads: ShouldSwap := FProcessList[i].Threads > FProcessList[j].Threads;
        scDiskRead: ShouldSwap := FProcessList[i].IOStats.ReadBytesPerSec > FProcessList[j].IOStats.ReadBytesPerSec;
        scDiskWrite: ShouldSwap := FProcessList[i].IOStats.WriteBytesPerSec > FProcessList[j].IOStats.WriteBytesPerSec;
        scTotalIO: ShouldSwap := FProcessList[i].IOStats.TotalIOPerSec > FProcessList[j].IOStats.TotalIOPerSec;
        scNetConnections: ShouldSwap := FProcessList[i].NetworkStats.TotalConnections > FProcessList[j].NetworkStats.TotalConnections;
        scPrivateMemory: ShouldSwap := FProcessList[i].MemBreakdown.PrivateMemory > FProcessList[j].MemBreakdown.PrivateMemory;
        scSharedMemory: ShouldSwap := FProcessList[i].MemBreakdown.SharedMemory > FProcessList[j].MemBreakdown.SharedMemory;
        scSwapUsage: ShouldSwap := FProcessList[i].MemBreakdown.SwapUsage > FProcessList[j].MemBreakdown.SwapUsage;
        scVirtualMemory: ShouldSwap := FProcessList[i].MemBreakdown.VmSize > FProcessList[j].MemBreakdown.VmSize;
        scSecurityRisk: ShouldSwap := FProcessList[i].SecurityInfo.RiskScore > FProcessList[j].SecurityInfo.RiskScore;
      end;
      
      if Descending then
        ShouldSwap := not ShouldSwap;
        
      if ShouldSwap then
      begin
        Temp := FProcessList[i];
        FProcessList[i] := FProcessList[j];
        FProcessList[j] := Temp;
      end;
    end;
  end;
end;

function TTaskManager.FilterByMinMemory(MinMemoryKB: Int64): TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].MemoryKB >= MinMemoryKB then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterByMinCPU(MinCPUPercent: Double): TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].CPUPercent >= MinCPUPercent then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterByMinDiskIO(MinBytesPerSec: Double): TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].IOStats.TotalIOPerSec >= MinBytesPerSec then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterByMinNetworkConnections(MinConnections: Integer): TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].NetworkStats.TotalConnections >= MinConnections then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterByMinPrivateMemory(MinPrivateKB: Int64): TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].MemBreakdown.PrivateMemory >= MinPrivateKB then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterByMinSharedMemory(MinSharedKB: Int64): TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].MemBreakdown.SharedMemory >= MinSharedKB then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterByMinSwap(MinSwapKB: Int64): TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].MemBreakdown.SwapUsage >= MinSwapKB then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterByMemoryType(PrivatePercent: Double): TProcessInfoArray;
var
  i, Count: Integer;
  TotalMem: Int64;
  ActualPercent: Double;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    TotalMem := FProcessList[i].MemBreakdown.PrivateMemory + FProcessList[i].MemBreakdown.SharedMemory;
    if TotalMem > 0 then
    begin
      ActualPercent := (FProcessList[i].MemBreakdown.PrivateMemory * 100.0) / TotalMem;
      if ActualPercent >= PrivatePercent then
      begin
        SetLength(Result, Count + 1);
        Result[Count] := FProcessList[i];
        Inc(Count);
      end;
    end;
  end;
end;

{ Security Filtering Functions }

function TTaskManager.FilterByRiskLevel(MinLevel: TSecurityRiskLevel): TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].SecurityInfo.RiskLevel >= MinLevel then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterByThreatType(ThreatType: TSecurityThreatType): TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if ThreatType in FProcessList[i].SecurityInfo.Threats then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterRootProcesses: TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].SecurityInfo.IsRoot then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterSuidProcesses: TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].SecurityInfo.IsSUID then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterSuspiciousProcesses: TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if (FProcessList[i].SecurityInfo.SuspiciousLocation) or 
       (FProcessList[i].SecurityInfo.SuspiciousName) or
       (FProcessList[i].SecurityInfo.ExecutableDeleted) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterDeletedExecutables: TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].SecurityInfo.ExecutableDeleted then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterPrivilegedPorts: TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].SecurityInfo.HasPrivilegedPort then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.FilterElevatedCapabilities: TProcessInfoArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    if FProcessList[i].SecurityInfo.Capabilities.HasElevatedCaps then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FProcessList[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.GetTopRiskyProcesses(Count: Integer): TProcessInfoArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  SortProcesses(scSecurityRisk, True);
  
  if Count > Length(FProcessList) then
    Count := Length(FProcessList);
    
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := FProcessList[i];
end;

function TTaskManager.GetSecurityEvents: TSecurityEventArray;
begin
  SetLength(Result, FSecurityEventCount);
  if FSecurityEventCount > 0 then
    Move(FSecurityEvents[0], Result[0], FSecurityEventCount * SizeOf(TSecurityEvent));
end;

function TTaskManager.GetSecurityEventsByRisk(MinLevel: TSecurityRiskLevel): TSecurityEventArray;
var
  i, Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  for i := 0 to FSecurityEventCount - 1 do
  begin
    if FSecurityEvents[i].RiskLevel >= MinLevel then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FSecurityEvents[i];
      Inc(Count);
    end;
  end;
end;

function TTaskManager.GetSystemSecurityStats: TSystemSecurityStats;
begin
  Result := FSystemStats.SecurityStats;
end;

function TTaskManager.GetTopProcessesByMemory(Count: Integer): TProcessInfoArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  SortProcesses(scMemory, True);
  
  if Count > Length(FProcessList) then
    Count := Length(FProcessList);
    
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := FProcessList[i];
end;

function TTaskManager.GetTopProcessesByCPU(Count: Integer): TProcessInfoArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  SortProcesses(scCPU, True);
  
  if Count > Length(FProcessList) then
    Count := Length(FProcessList);
    
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := FProcessList[i];
end;

function TTaskManager.GetTopProcessesByDiskRead(Count: Integer): TProcessInfoArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  SortProcesses(scDiskRead, True);
  
  if Count > Length(FProcessList) then
    Count := Length(FProcessList);
    
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := FProcessList[i];
end;

function TTaskManager.GetTopProcessesByDiskWrite(Count: Integer): TProcessInfoArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  SortProcesses(scDiskWrite, True);
  
  if Count > Length(FProcessList) then
    Count := Length(FProcessList);
    
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := FProcessList[i];
end;

function TTaskManager.GetTopProcessesByTotalIO(Count: Integer): TProcessInfoArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  SortProcesses(scTotalIO, True);
  
  if Count > Length(FProcessList) then
    Count := Length(FProcessList);
    
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := FProcessList[i];
end;

function TTaskManager.GetTopProcessesByNetworkConnections(Count: Integer): TProcessInfoArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  SortProcesses(scNetConnections, True);
  
  if Count > Length(FProcessList) then
    Count := Length(FProcessList);
    
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := FProcessList[i];
end;

function TTaskManager.GetTopProcessesByPrivateMemory(Count: Integer): TProcessInfoArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  SortProcesses(scPrivateMemory, True);
  
  if Count > Length(FProcessList) then
    Count := Length(FProcessList);
    
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := FProcessList[i];
end;

function TTaskManager.GetTopProcessesBySharedMemory(Count: Integer): TProcessInfoArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  SortProcesses(scSharedMemory, True);
  
  if Count > Length(FProcessList) then
    Count := Length(FProcessList);
    
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := FProcessList[i];
end;

function TTaskManager.GetTopProcessesBySwap(Count: Integer): TProcessInfoArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  SortProcesses(scSwapUsage, True);
  
  if Count > Length(FProcessList) then
    Count := Length(FProcessList);
    
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := FProcessList[i];
end;

function TTaskManager.GetTopProcessesByVirtualMemory(Count: Integer): TProcessInfoArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  SortProcesses(scVirtualMemory, True);
  
  if Count > Length(FProcessList) then
    Count := Length(FProcessList);
    
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := FProcessList[i];
end;

function TTaskManager.GetMemInfo: Boolean;
var
  Lines: TStringArray;
  i: Integer;
  Line, Key, Value: string;
  ColonPos: Integer;
begin
  Result := False;
  Lines := ReadFileToStrings('/proc/meminfo');
  
  FSystemStats.TotalMemoryKB := 0;
  FSystemStats.FreeMemoryKB := 0;
  FSystemStats.CachedMemoryKB := 0;
  
  for i := 0 to Length(Lines) - 1 do
  begin
    Line := Lines[i];
    ColonPos := Pos(':', Line);
    if ColonPos > 0 then
    begin
      Key := Trim(Copy(Line, 1, ColonPos - 1));
      Value := Trim(Copy(Line, ColonPos + 1, Length(Line)));
      Value := Trim(StringReplace(Value, ' kB', '', [rfReplaceAll]));
      
      if Key = 'MemTotal' then
        FSystemStats.TotalMemoryKB := StrToInt64Def(Value, 0)
      else if Key = 'MemFree' then
        FSystemStats.FreeMemoryKB := StrToInt64Def(Value, 0)
      else if Key = 'Cached' then
        FSystemStats.CachedMemoryKB := StrToInt64Def(Value, 0);
    end;
  end;
  
  FSystemStats.UsedMemoryKB := FSystemStats.TotalMemoryKB - FSystemStats.FreeMemoryKB;
  Result := FSystemStats.TotalMemoryKB > 0;
end;

function TTaskManager.GetLoadAverage: Boolean;
var
  Content: string;
  Parts: TStringArray;
  i, Count: Integer;
  CurrentPart: string;
begin
  Result := False;
  Content := ReadFileToString('/proc/loadavg');
  if Content = '' then
    Exit;
    
  SetLength(Parts, 0);
  Count := 0;
  CurrentPart := '';
  
  for i := 1 to Length(Content) do
  begin
    if (Content[i] = ' ') or (Content[i] = #10) or (Content[i] = #13) then
    begin
      if CurrentPart <> '' then
      begin
        SetLength(Parts, Count + 1);
        Parts[Count] := CurrentPart;
        Inc(Count);
        CurrentPart := '';
      end;
    end
    else
      CurrentPart := CurrentPart + Content[i];
  end;
  
  if Count >= 3 then
  begin
    FSystemStats.LoadAverage1 := StrToFloatDef(Parts[0], 0.0);
    FSystemStats.LoadAverage5 := StrToFloatDef(Parts[1], 0.0);
    FSystemStats.LoadAverage15 := StrToFloatDef(Parts[2], 0.0);
    Result := True;
  end;
end;

function TTaskManager.GetUptime: Boolean;
var
  Content: string;
  Parts: TStringArray;
  i, Count: Integer;
  CurrentPart: string;
begin
  Result := False;
  Content := ReadFileToString('/proc/uptime');
  if Content = '' then
    Exit;
    
  SetLength(Parts, 0);
  Count := 0;
  CurrentPart := '';
  
  for i := 1 to Length(Content) do
  begin
    if (Content[i] = ' ') or (Content[i] = #10) or (Content[i] = #13) then
    begin
      if CurrentPart <> '' then
      begin
        SetLength(Parts, Count + 1);
        Parts[Count] := CurrentPart;
        Inc(Count);
        CurrentPart := '';
      end;
    end
    else
      CurrentPart := CurrentPart + Content[i];
  end;
  
  if Count >= 1 then
  begin
    FSystemStats.UptimeSeconds := Trunc(StrToFloatDef(Parts[0], 0.0));
    Result := True;
  end;
end;

procedure TTaskManager.UpdateSystemStats;
var
  i: Integer;
begin
  FSystemStats.TotalProcesses := Length(FProcessList);
  FSystemStats.RunningProcesses := 0;
  FSystemStats.SleepingProcesses := 0;
  FSystemStats.ZombieProcesses := 0;
  FSystemStats.TotalDiskReadBytes := 0;
  FSystemStats.TotalDiskWriteBytes := 0;
  FSystemStats.TotalNetworkConnections := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    case FProcessList[i].State of
      psRunning: Inc(FSystemStats.RunningProcesses);
      psSleeping: Inc(FSystemStats.SleepingProcesses);
      psZombie: Inc(FSystemStats.ZombieProcesses);
    end;
    
    FSystemStats.TotalDiskReadBytes := FSystemStats.TotalDiskReadBytes + FProcessList[i].IOStats.ReadBytes;
    FSystemStats.TotalDiskWriteBytes := FSystemStats.TotalDiskWriteBytes + FProcessList[i].IOStats.WriteBytes;
    FSystemStats.TotalNetworkConnections := FSystemStats.TotalNetworkConnections + FProcessList[i].NetworkStats.TotalConnections;
  end;
  
  GetMemInfo;
  GetLoadAverage;
  GetUptime;
  
  FSystemStats.CPUCount := 1;
end;

procedure TTaskManager.UpdateSystemMemoryBreakdown;
var
  i: Integer;
  MaxPrivate, MaxShared: Int64;
  ProcessesWithSwap: Integer;
begin
  FillChar(FSystemStats.MemBreakdown, SizeOf(FSystemStats.MemBreakdown), 0);
  
  MaxPrivate := 0;
  MaxShared := 0;
  ProcessesWithSwap := 0;
  
  for i := 0 to Length(FProcessList) - 1 do
  begin
    with FProcessList[i].MemBreakdown do
    begin
      FSystemStats.MemBreakdown.TotalPrivateMemoryKB := FSystemStats.MemBreakdown.TotalPrivateMemoryKB + PrivateMemory;
      FSystemStats.MemBreakdown.TotalSharedMemoryKB := FSystemStats.MemBreakdown.TotalSharedMemoryKB + SharedMemory;
      FSystemStats.MemBreakdown.TotalSwapUsageKB := FSystemStats.MemBreakdown.TotalSwapUsageKB + SwapUsage;
      FSystemStats.MemBreakdown.TotalVirtualMemoryKB := FSystemStats.MemBreakdown.TotalVirtualMemoryKB + VmSize;
      FSystemStats.MemBreakdown.TotalResidentMemoryKB := FSystemStats.MemBreakdown.TotalResidentMemoryKB + VmRSS;
      
      if SwapUsage > 0 then
        Inc(ProcessesWithSwap);
        
      if PrivateMemory > MaxPrivate then
        MaxPrivate := PrivateMemory;
        
      if SharedMemory > MaxShared then
        MaxShared := SharedMemory;
    end;
  end;
  
  FSystemStats.MemBreakdown.ProcessesUsingSwap := ProcessesWithSwap;
  FSystemStats.MemBreakdown.LargestPrivateMemoryKB := MaxPrivate;
  FSystemStats.MemBreakdown.LargestSharedMemoryKB := MaxShared;
  
  if Length(FProcessList) > 0 then
  begin
    FSystemStats.MemBreakdown.AveragePrivateMemoryKB := FSystemStats.MemBreakdown.TotalPrivateMemoryKB div Length(FProcessList);
    FSystemStats.MemBreakdown.AverageSharedMemoryKB := FSystemStats.MemBreakdown.TotalSharedMemoryKB div Length(FProcessList);
  end;
end;

function TTaskManager.GetSystemStats: TSystemStats;
begin
  Result := FSystemStats;
end;

function TTaskManager.GetTotalMemoryUsage: Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FProcessList) - 1 do
    Result := Result + FProcessList[i].MemoryKB;
end;

function TTaskManager.GetProcessCount: Integer;
begin
  Result := Length(FProcessList);
end;

function TTaskManager.GetTotalDiskReadRate: Double;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to Length(FProcessList) - 1 do
    Result := Result + FProcessList[i].IOStats.ReadBytesPerSec;
end;

function TTaskManager.GetTotalDiskWriteRate: Double;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to Length(FProcessList) - 1 do
    Result := Result + FProcessList[i].IOStats.WriteBytesPerSec;
end;

function TTaskManager.GetTotalNetworkConnections: Integer;
begin
  Result := FSystemStats.TotalNetworkConnections;
end;

function TTaskManager.GetSystemMemoryBreakdown: TSystemMemoryBreakdown;
begin
  Result := FSystemStats.MemBreakdown;
end;

function TTaskManager.GetTotalPrivateMemory: Int64;
begin
  Result := FSystemStats.MemBreakdown.TotalPrivateMemoryKB;
end;

function TTaskManager.GetTotalSharedMemory: Int64;
begin
  Result := FSystemStats.MemBreakdown.TotalSharedMemoryKB;
end;

function TTaskManager.GetTotalSwapUsage: Int64;
begin
  Result := FSystemStats.MemBreakdown.TotalSwapUsageKB;
end;

function TTaskManager.FormatBytes(Bytes: Int64): string;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
  TB = Int64(1024) * GB;
begin
  if Bytes >= TB then
    Result := Format('%.2f TB', [Bytes / TB])
  else if Bytes >= GB then
    Result := Format('%.2f GB', [Bytes / GB])
  else if Bytes >= MB then
    Result := Format('%.2f MB', [Bytes / MB])
  else if Bytes >= KB then
    Result := Format('%.2f KB', [Bytes / KB])
  else
    Result := Format('%d B', [Bytes]);
end;

function TTaskManager.FormatBytesPerSec(BytesPerSec: Double): string;
const
  KB = 1024.0;
  MB = 1024.0 * KB;
  GB = 1024.0 * MB;
begin
  if BytesPerSec >= GB then
    Result := Format('%.2f GB/s', [BytesPerSec / GB])
  else if BytesPerSec >= MB then
    Result := Format('%.2f MB/s', [BytesPerSec / MB])
  else if BytesPerSec >= KB then
    Result := Format('%.2f KB/s', [BytesPerSec / KB])
  else
    Result := Format('%.0f B/s', [BytesPerSec]);
end;

function TTaskManager.ProcessInfoToString(const Info: TProcessInfo): string;
begin
  Result := Format('PID: %6d | Name: %-20s | State: %-12s | CPU: %6.2f%% | Mem: %8d KB | Threads: %3d',
    [Info.PID, Copy(Info.Name, 1, 20), StateEnumToString(Info.State),
     Info.CPUPercent, Info.MemoryKB, Info.Threads]);
end;

function TTaskManager.ProcessInfoToStringWithIO(const Info: TProcessInfo): string;
begin
  Result := Format('PID: %6d | %-16s | R: %10s | W: %10s | Net: %3d | Mem: %8s',
    [Info.PID, 
     Copy(Info.Name, 1, 16),
     FormatBytesPerSec(Info.IOStats.ReadBytesPerSec),
     FormatBytesPerSec(Info.IOStats.WriteBytesPerSec),
     Info.NetworkStats.TotalConnections,
     FormatBytes(Info.MemoryKB * 1024)]);
end;

function TTaskManager.MemoryBreakdownToString(const MemBreakdown: TMemoryBreakdown): string;
begin
  Result := Format('RSS: %s | Private: %s | Shared: %s | Swap: %s | Virtual: %s',
    [FormatBytes(MemBreakdown.TotalRSS * 1024),
     FormatBytes(MemBreakdown.PrivateMemory * 1024),
     FormatBytes(MemBreakdown.SharedMemory * 1024),
     FormatBytes(MemBreakdown.SwapUsage * 1024),
     FormatBytes(MemBreakdown.VmSize * 1024)]);
end;

function TTaskManager.ProcessInfoToStringWithMemory(const Info: TProcessInfo): string;
var
  PrivatePercent, SharedPercent: Double;
  TotalMem: Int64;
begin
  TotalMem := Info.MemBreakdown.PrivateMemory + Info.MemBreakdown.SharedMemory;
  if TotalMem > 0 then
  begin
    PrivatePercent := (Info.MemBreakdown.PrivateMemory * 100.0) / TotalMem;
    SharedPercent := (Info.MemBreakdown.SharedMemory * 100.0) / TotalMem;
  end
  else
  begin
    PrivatePercent := 0.0;
    SharedPercent := 0.0;
  end;
  
  Result := Format('PID: %6d | %-16s | Priv: %8s (%5.1f%%) | Shar: %8s (%5.1f%%) | Swap: %8s',
    [Info.PID,
     Copy(Info.Name, 1, 16),
     FormatBytes(Info.MemBreakdown.PrivateMemory * 1024),
     PrivatePercent,
     FormatBytes(Info.MemBreakdown.SharedMemory * 1024),
     SharedPercent,
     FormatBytes(Info.MemBreakdown.SwapUsage * 1024)]);
end;

function TTaskManager.ProcessMemoryDetailsToString(const Info: TProcessInfo): string;
begin
  Result := Format('Process: %s (PID: %d)', [Info.Name, Info.PID]) + #10;
  Result := Result + Format('  Virtual Memory (VmSize):    %s', [FormatBytes(Info.MemBreakdown.VmSize * 1024)]) + #10;
  Result := Result + Format('  Resident Set (VmRSS):       %s', [FormatBytes(Info.MemBreakdown.VmRSS * 1024)]) + #10;
  Result := Result + Format('  Peak Virtual (VmPeak):      %s', [FormatBytes(Info.MemBreakdown.VmPeak * 1024)]) + #10;
  Result := Result + Format('  Peak Resident (VmHWM):      %s', [FormatBytes(Info.MemBreakdown.VmHWM * 1024)]) + #10;
  Result := Result + '  --- Memory Breakdown ---' + #10;
  Result := Result + Format('  Private (RssAnon):          %s', [FormatBytes(Info.MemBreakdown.RssAnon * 1024)]) + #10;
  Result := Result + Format('  File-backed (RssFile):      %s', [FormatBytes(Info.MemBreakdown.RssFile * 1024)]) + #10;
  Result := Result + Format('  Shared Memory (RssShmem):   %s', [FormatBytes(Info.MemBreakdown.RssShmem * 1024)]) + #10;
  Result := Result + Format('  Data Segment (VmData):      %s', [FormatBytes(Info.MemBreakdown.VmData * 1024)]) + #10;
  Result := Result + Format('  Stack (VmStk):              %s', [FormatBytes(Info.MemBreakdown.VmStk * 1024)]) + #10;
  Result := Result + Format('  Executable (VmExe):         %s', [FormatBytes(Info.MemBreakdown.VmExe * 1024)]) + #10;
  Result := Result + Format('  Libraries (VmLib):          %s', [FormatBytes(Info.MemBreakdown.VmLib * 1024)]) + #10;
  Result := Result + Format('  Page Tables (VmPTE):        %s', [FormatBytes(Info.MemBreakdown.VmPTE * 1024)]) + #10;
  Result := Result + Format('  Swap Usage (VmSwap):        %s', [FormatBytes(Info.MemBreakdown.VmSwap * 1024)]) + #10;
  Result := Result + Format('  Locked Memory (VmLck):      %s', [FormatBytes(Info.MemBreakdown.VmLock * 1024)]) + #10;
end;

{ Security Display Functions }

function TTaskManager.SecurityInfoToString(const SecInfo: TSecurityInfo): string;
begin
  Result := Format('Risk: %-12s | Score: %3d | UID: %4d | Root: %5s | SUID: %5s | Threats: %s',
    [RiskLevelToString(SecInfo.RiskLevel),
     SecInfo.RiskScore,
     SecInfo.UID,
     BoolToStr(SecInfo.IsRoot, 'Yes', 'No'),
     BoolToStr(SecInfo.IsSUID, 'Yes', 'No'),
     Copy(SecInfo.ThreatDescription, 1, 40)]);
end;

function TTaskManager.ProcessSecurityDetailsToString(const Info: TProcessInfo): string;
var
  i: Integer;
begin
  Result := Format('Process: %s (PID: %d)', [Info.Name, Info.PID]) + #10;
  Result := Result + Format('  Security Risk Level: %s (Score: %d/100)', 
    [RiskLevelToString(Info.SecurityInfo.RiskLevel), Info.SecurityInfo.RiskScore]) + #10;
  Result := Result + '  --- Identity ---' + #10;
  Result := Result + Format('  UID: %d | EUID: %d | GID: %d | EGID: %d',
    [Info.SecurityInfo.UID, Info.SecurityInfo.EUID, Info.SecurityInfo.GID, Info.SecurityInfo.EGID]) + #10;
  Result := Result + Format('  Running as Root: %s', [BoolToStr(Info.SecurityInfo.IsRoot, 'YES', 'No')]) + #10;
  Result := Result + Format('  SUID Process: %s', [BoolToStr(Info.SecurityInfo.IsSUID, 'YES', 'No')]) + #10;
  Result := Result + Format('  SGID Process: %s', [BoolToStr(Info.SecurityInfo.IsSGID, 'YES', 'No')]) + #10;
  Result := Result + '  --- Executable ---' + #10;
  Result := Result + Format('  Path: %s', [Info.SecurityInfo.ExecutablePath]) + #10;
  Result := Result + Format('  Deleted: %s', [BoolToStr(Info.SecurityInfo.ExecutableDeleted, 'YES (SUSPICIOUS!)', 'No')]) + #10;
  Result := Result + Format('  Suspicious Location: %s', [BoolToStr(Info.SecurityInfo.SuspiciousLocation, 'YES', 'No')]) + #10;
  Result := Result + '  --- Capabilities ---' + #10;
  Result := Result + Format('  Effective: %s', [HexToCapabilities(Info.SecurityInfo.Capabilities.CapEffective)]) + #10;
  Result := Result + Format('  Permitted: %s', [HexToCapabilities(Info.SecurityInfo.Capabilities.CapPermitted)]) + #10;
  Result := Result + Format('  Elevated Caps: %s', [BoolToStr(Info.SecurityInfo.Capabilities.HasElevatedCaps, 'YES', 'No')]) + #10;
  Result := Result + '  --- Network ---' + #10;
  Result := Result + Format('  Listening Ports: %d', [Length(Info.SecurityInfo.ListeningPorts)]);
  if Length(Info.SecurityInfo.ListeningPorts) > 0 then
  begin
    Result := Result + ' [';
    for i := 0 to Min(4, Length(Info.SecurityInfo.ListeningPorts) - 1) do
    begin
      Result := Result + IntToStr(Info.SecurityInfo.ListeningPorts[i]);
      if i < Min(4, Length(Info.SecurityInfo.ListeningPorts) - 1) then
        Result := Result + ', ';
    end;
    if Length(Info.SecurityInfo.ListeningPorts) > 5 then
      Result := Result + ', ...';
    Result := Result + ']';
  end;
  Result := Result + #10;
  Result := Result + Format('  Privileged Port: %s', [BoolToStr(Info.SecurityInfo.HasPrivilegedPort, 'YES', 'No')]) + #10;
  Result := Result + Format('  Raw Socket: %s', [BoolToStr(Info.SecurityInfo.HasRawSocket, 'YES', 'No')]) + #10;
  Result := Result + Format('  Listening All Interfaces: %s', [BoolToStr(Info.SecurityInfo.ListeningAllInterfaces, 'YES', 'No')]) + #10;
  Result := Result + '  --- Threats Detected ---' + #10;
  Result := Result + Format('  %s', [Info.SecurityInfo.ThreatDescription]) + #10;
end;

function TTaskManager.SecurityEventToString(const Event: TSecurityEvent): string;
begin
  Result := Format('%s | PID: %6d | %-16s | %s | %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Event.Timestamp),
     Event.PID,
     Copy(Event.ProcessName, 1, 16),
     RiskLevelToString(Event.RiskLevel),
     Event.Description]);
end;

function TTaskManager.SystemSecurityStatsToString: string;
var
  Stats: TSystemSecurityStats;
begin
  Stats := FSystemStats.SecurityStats;
  
  Result := '';
  Result := Result + '========== SYSTEM SECURITY STATISTICS ==========' + #10;
  Result := Result + Format('Overall Security Score: %d/100', [Stats.OverallSecurityScore]);
  if Stats.OverallSecurityScore >= 90 then
    Result := Result + ' (EXCELLENT)'
  else if Stats.OverallSecurityScore >= 75 then
    Result := Result + ' (GOOD)'
  else if Stats.OverallSecurityScore >= 50 then
    Result := Result + ' (FAIR)'
  else if Stats.OverallSecurityScore >= 25 then
    Result := Result + ' (POOR)'
  else
    Result := Result + ' (CRITICAL)';
  Result := Result + #10;
  Result := Result + Format('Total Processes: %d', [Stats.TotalProcesses]) + #10;
  Result := Result + Format('  Safe: %d | Low Risk: %d | Medium Risk: %d | High Risk: %d | Critical: %d',
    [Stats.SafeProcesses, Stats.LowRiskProcesses, Stats.MediumRiskProcesses,
     Stats.HighRiskProcesses, Stats.CriticalRiskProcesses]) + #10;
  Result := Result + Format('Root Processes: %d', [Stats.RootProcesses]) + #10;
  Result := Result + Format('SUID Processes: %d', [Stats.SuidProcesses]) + #10;
  Result := Result + Format('Processes with Privileged Ports: %d', [Stats.ProcessesWithPrivilegedPorts]) + #10;
  Result := Result + Format('Processes with Elevated Capabilities: %d', [Stats.ProcessesWithElevatedCaps]) + #10;
  Result := Result + Format('Suspicious Location Processes: %d', [Stats.SuspiciousLocationProcesses]) + #10;
  Result := Result + Format('Deleted Executable Processes: %d', [Stats.DeletedExecutableProcesses]) + #10;
  Result := Result + Format('Total Threats Detected: %d', [Stats.TotalThreatsDetected]) + #10;
  Result := Result + '================================================' + #10;
end;

function TTaskManager.SystemStatsToString: string;
begin
  Result := '';
  Result := Result + '========== SYSTEM STATISTICS ==========' + #10;
  Result := Result + Format('Total Processes: %d', [FSystemStats.TotalProcesses]) + #10;
  Result := Result + Format('Running: %d | Sleeping: %d | Zombie: %d',
    [FSystemStats.RunningProcesses, FSystemStats.SleepingProcesses,
     FSystemStats.ZombieProcesses]) + #10;
  Result := Result + Format('Total Memory: %d MB | Used: %d MB | Free: %d MB | Cached: %d MB',
    [FSystemStats.TotalMemoryKB div 1024, FSystemStats.UsedMemoryKB div 1024,
     FSystemStats.FreeMemoryKB div 1024, FSystemStats.CachedMemoryKB div 1024]) + #10;
  Result := Result + Format('Total Disk Read: %s | Total Disk Write: %s',
    [FormatBytes(FSystemStats.TotalDiskReadBytes), FormatBytes(FSystemStats.TotalDiskWriteBytes)]) + #10;
  Result := Result + Format('Total Network Connections: %d', [FSystemStats.TotalNetworkConnections]) + #10;
  Result := Result + Format('Load Average: %.2f, %.2f, %.2f',
    [FSystemStats.LoadAverage1, FSystemStats.LoadAverage5, FSystemStats.LoadAverage15]) + #10;
  Result := Result + Format('Uptime: %d seconds (%d days, %d hours)',
    [FSystemStats.UptimeSeconds, FSystemStats.UptimeSeconds div 86400,
     (FSystemStats.UptimeSeconds mod 86400) div 3600]) + #10;
  Result := Result + '=======================================' + #10;
end;

procedure TTaskManager.PrintProcessList;
var
  i: Integer;
begin
  WriteLn('========== PROCESS LIST ==========');
  for i := 0 to Length(FProcessList) - 1 do
    WriteLn(ProcessInfoToString(FProcessList[i]));
  WriteLn('==================================');
end;

procedure TTaskManager.PrintProcessListWithIO;
var
  i: Integer;
begin
  WriteLn('========== PROCESS LIST WITH I/O STATISTICS ==========');
  WriteLn('PID      | Name             | Disk Read  | Disk Write | Net | Memory');
  WriteLn('---------|------------------|------------|------------|-----|----------');
  for i := 0 to Min(20, Length(FProcessList) - 1) do
    WriteLn(ProcessInfoToStringWithIO(FProcessList[i]));
  WriteLn('======================================================');
end;

procedure TTaskManager.PrintProcessListWithMemoryBreakdown;
var
  i: Integer;
begin
  WriteLn('========== PROCESS LIST WITH MEMORY BREAKDOWN ==========');
  WriteLn('PID      | Name             | Private Memory | Shared Memory  | Swap Usage');
  WriteLn('---------|------------------|----------------|----------------|------------');
  for i := 0 to Min(20, Length(FProcessList) - 1) do
    WriteLn(ProcessInfoToStringWithMemory(FProcessList[i]));
  WriteLn('========================================================');
end;

procedure TTaskManager.PrintTopMemoryProcesses(Count: Integer);
var
  TopPrivate, TopShared, TopSwap: TProcessInfoArray;
  i: Integer;
begin
  WriteLn('========== TOP PROCESSES BY PRIVATE MEMORY ==========');
  TopPrivate := GetTopProcessesByPrivateMemory(Count);
  for i := 0 to Length(TopPrivate) - 1 do
    WriteLn(ProcessInfoToStringWithMemory(TopPrivate[i]));
    
  WriteLn('');
  WriteLn('========== TOP PROCESSES BY SHARED MEMORY ==========');
  TopShared := GetTopProcessesBySharedMemory(Count);
  for i := 0 to Length(TopShared) - 1 do
    WriteLn(ProcessInfoToStringWithMemory(TopShared[i]));
    
  WriteLn('');
  WriteLn('========== TOP PROCESSES BY SWAP USAGE ==========');
  TopSwap := GetTopProcessesBySwap(Count);
  for i := 0 to Length(TopSwap) - 1 do
    WriteLn(ProcessInfoToStringWithMemory(TopSwap[i]));
  WriteLn('==================================================');
end;

procedure TTaskManager.PrintMemoryBreakdownStatistics;
var
  MemBD: TSystemMemoryBreakdown;
begin
  MemBD := FSystemStats.MemBreakdown;
  
  WriteLn('========== SYSTEM MEMORY BREAKDOWN STATISTICS ==========');
  WriteLn(Format('Total Private Memory:       %s', [FormatBytes(MemBD.TotalPrivateMemoryKB * 1024)]));
  WriteLn(Format('Total Shared Memory:        %s', [FormatBytes(MemBD.TotalSharedMemoryKB * 1024)]));
  WriteLn(Format('Total Swap Usage:           %s', [FormatBytes(MemBD.TotalSwapUsageKB * 1024)]));
  WriteLn(Format('Total Virtual Memory:       %s', [FormatBytes(MemBD.TotalVirtualMemoryKB * 1024)]));
  WriteLn(Format('Total Resident Memory:      %s', [FormatBytes(MemBD.TotalResidentMemoryKB * 1024)]));
  WriteLn(Format('Processes Using Swap:       %d', [MemBD.ProcessesUsingSwap]));
  WriteLn(Format('Largest Private Memory:     %s', [FormatBytes(MemBD.LargestPrivateMemoryKB * 1024)]));
  WriteLn(Format('Largest Shared Memory:      %s', [FormatBytes(MemBD.LargestSharedMemoryKB * 1024)]));
  WriteLn(Format('Average Private Memory:     %s', [FormatBytes(MemBD.AveragePrivateMemoryKB * 1024)]));
  WriteLn(Format('Average Shared Memory:      %s', [FormatBytes(MemBD.AverageSharedMemoryKB * 1024)]));
  WriteLn('========================================================');
end;

procedure TTaskManager.PrintDetailedMemoryAnalysis(Count: Integer);
var
  TopProcesses: TProcessInfoArray;
  i: Integer;
begin
  WriteLn('========== DETAILED MEMORY ANALYSIS ==========');
  WriteLn('');
  
  TopProcesses := GetTopProcessesByMemory(Count);
  for i := 0 to Length(TopProcesses) - 1 do
  begin
    WriteLn(ProcessMemoryDetailsToString(TopProcesses[i]));
    if i < Length(TopProcesses) - 1 then
      WriteLn('');
  end;
  WriteLn('==============================================');
end;

procedure TTaskManager.PrintSystemStats;
begin
  WriteLn(SystemStatsToString);
end;

procedure TTaskManager.PrintIOStatistics;
var
  TotalReadRate, TotalWriteRate: Double;
begin
  TotalReadRate := GetTotalDiskReadRate;
  TotalWriteRate := GetTotalDiskWriteRate;
  
  WriteLn('========== I/O STATISTICS ==========');
  WriteLn(Format('System-wide Disk Read Rate:  %s', [FormatBytesPerSec(TotalReadRate)]));
  WriteLn(Format('System-wide Disk Write Rate: %s', [FormatBytesPerSec(TotalWriteRate)]));
  WriteLn(Format('Total I/O Rate:              %s', [FormatBytesPerSec(TotalReadRate + TotalWriteRate)]));
  WriteLn(Format('Total Disk Read (cumulative):  %s', [FormatBytes(FSystemStats.TotalDiskReadBytes)]));
  WriteLn(Format('Total Disk Write (cumulative): %s', [FormatBytes(FSystemStats.TotalDiskWriteBytes)]));
  WriteLn(Format('Total Network Connections:     %d', [FSystemStats.TotalNetworkConnections]));
  WriteLn('====================================');
end;

procedure TTaskManager.PrintTopProcesses(Count: Integer);
var
  TopMem, TopCPU: TProcessInfoArray;
  i: Integer;
begin
  WriteLn('========== TOP PROCESSES BY MEMORY ==========');
  TopMem := GetTopProcessesByMemory(Count);
  for i := 0 to Length(TopMem) - 1 do
    WriteLn(ProcessInfoToString(TopMem[i]));
    
  WriteLn('');
  WriteLn('========== TOP PROCESSES BY CPU ==========');
  TopCPU := GetTopProcessesByCPU(Count);
  for i := 0 to Length(TopCPU) - 1 do
    WriteLn(ProcessInfoToString(TopCPU[i]));
  WriteLn('==========================================');
end;

procedure TTaskManager.PrintTopIOProcesses(Count: Integer);
var
  TopRead, TopWrite, TopTotal, TopNet: TProcessInfoArray;
  i: Integer;
begin
  WriteLn('========== TOP PROCESSES BY DISK READ ==========');
  TopRead := GetTopProcessesByDiskRead(Count);
  for i := 0 to Length(TopRead) - 1 do
    WriteLn(ProcessInfoToStringWithIO(TopRead[i]));
    
  WriteLn('');
  WriteLn('========== TOP PROCESSES BY DISK WRITE ==========');
  TopWrite := GetTopProcessesByDiskWrite(Count);
  for i := 0 to Length(TopWrite) - 1 do
    WriteLn(ProcessInfoToStringWithIO(TopWrite[i]));
    
  WriteLn('');
  WriteLn('========== TOP PROCESSES BY TOTAL I/O ==========');
  TopTotal := GetTopProcessesByTotalIO(Count);
  for i := 0 to Length(TopTotal) - 1 do
    WriteLn(ProcessInfoToStringWithIO(TopTotal[i]));
    
  WriteLn('');
  WriteLn('========== TOP PROCESSES BY NETWORK CONNECTIONS ==========');
  TopNet := GetTopProcessesByNetworkConnections(Count);
  for i := 0 to Length(TopNet) - 1 do
    WriteLn(ProcessInfoToStringWithIO(TopNet[i]));
  WriteLn('=========================================================');
end;

{ Security Printing Functions }

procedure TTaskManager.PrintSecurityAnalysis;
var
  i: Integer;
begin
  WriteLn('========== SECURITY ANALYSIS ==========');
  WriteLn('PID      | Name             | Risk Level   | Score | Threats');
  WriteLn('---------|------------------|--------------|-------|------------------------------------------');
  for i := 0 to Min(20, Length(FProcessList) - 1) do
  begin
    WriteLn(Format('%-8d | %-16s | %-12s | %5d | %s',
      [FProcessList[i].PID,
       Copy(FProcessList[i].Name, 1, 16),
       RiskLevelToString(FProcessList[i].SecurityInfo.RiskLevel),
       FProcessList[i].SecurityInfo.RiskScore,
       Copy(FProcessList[i].SecurityInfo.ThreatDescription, 1, 40)]));
  end;
  WriteLn('=======================================');
end;

procedure TTaskManager.PrintSecurityThreats;
var
  Risky: TProcessInfoArray;
  i: Integer;
begin
  Risky := FilterByRiskLevel(srlMediumRisk);
  
  WriteLn('========== SECURITY THREATS DETECTED ==========');
  if Length(Risky) = 0 then
  begin
    WriteLn('No significant threats detected. System appears secure.');
  end
  else
  begin
    WriteLn(Format('Found %d processes with medium or higher risk level:', [Length(Risky)]));
    WriteLn('');
    for i := 0 to Min(9, Length(Risky) - 1) do
    begin
      WriteLn(Format('PID %d: %s', [Risky[i].PID, Risky[i].Name]));
      WriteLn(Format('  Risk: %s (Score: %d/100)', 
        [RiskLevelToString(Risky[i].SecurityInfo.RiskLevel), Risky[i].SecurityInfo.RiskScore]));
      WriteLn(Format('  Threats: %s', [Risky[i].SecurityInfo.ThreatDescription]));
      if Risky[i].SecurityInfo.ExecutablePath <> '' then
        WriteLn(Format('  Path: %s', [Risky[i].SecurityInfo.ExecutablePath]));
      WriteLn('');
    end;
  end;
  WriteLn('===============================================');
end;

procedure TTaskManager.PrintRiskyProcesses(Count: Integer);
var
  Risky: TProcessInfoArray;
  i: Integer;
begin
  WriteLn('========== TOP RISKY PROCESSES ==========');
  Risky := GetTopRiskyProcesses(Count);
  for i := 0 to Length(Risky) - 1 do
    WriteLn(SecurityInfoToString(Risky[i].SecurityInfo));
  WriteLn('=========================================');
end;

procedure TTaskManager.PrintSecurityEvents(Count: Integer);
var
  Events: TSecurityEventArray;
  i, StartIdx: Integer;
begin
  WriteLn('========== SECURITY EVENTS LOG ==========');
  Events := GetSecurityEvents;
  
  if Length(Events) = 0 then
  begin
    WriteLn('No security events logged.');
  end
  else
  begin
    WriteLn(Format('Total Events: %d | Showing last %d:', [Length(Events), Min(Count, Length(Events))]));
    WriteLn('');
    StartIdx := Max(0, Length(Events) - Count);
    for i := StartIdx to Length(Events) - 1 do
      WriteLn(SecurityEventToString(Events[i]));
  end;
  WriteLn('=========================================');
end;

procedure TTaskManager.PrintSecurityStatistics;
begin
  WriteLn(SystemSecurityStatsToString);
end;

procedure TTaskManager.PrintDetailedSecurityAnalysis(Count: Integer);
var
  TopRisky: TProcessInfoArray;
  i: Integer;
begin
  WriteLn('========== DETAILED SECURITY ANALYSIS ==========');
  WriteLn('');
  
  TopRisky := GetTopRiskyProcesses(Count);
  for i := 0 to Length(TopRisky) - 1 do
  begin
    WriteLn(ProcessSecurityDetailsToString(TopRisky[i]));
    if i < Length(TopRisky) - 1 then
      WriteLn('');
  end;
  WriteLn('================================================');
end;

{ Security Export Functions }

function TTaskManager.ExportSecurityAnalysisToCSV(const FileName: string): Boolean;
var
  F: TextFile;
  i, j: Integer;
  PortsList: string;
begin
  Result := False;
  try
    AssignFile(F, FileName);
    Rewrite(F);
    try
      WriteLn(F, 'PID,Name,RiskLevel,RiskScore,UID,EUID,IsRoot,IsSUID,IsSGID,ExecutablePath,ExecutableDeleted,SuspiciousLocation,SuspiciousName,HasElevatedCaps,ListeningPorts,HasPrivilegedPort,HasRawSocket,ThreatDescription');
      
      for i := 0 to Length(FProcessList) - 1 do
      begin
        with FProcessList[i] do
        begin
          PortsList := '';
          for j := 0 to Length(SecurityInfo.ListeningPorts) - 1 do
          begin
            PortsList := PortsList + IntToStr(SecurityInfo.ListeningPorts[j]);
            if j < Length(SecurityInfo.ListeningPorts)  - 1 then
              PortsList := PortsList + ';';
          end;
          
          WriteLn(F, Format('%d,"%s",%d,%d,%d,%d,%d,%d,%d,"%s",%d,%d,%d,%d,"%s",%d,%d,"%s"',
            [PID, Name,
             Ord(SecurityInfo.RiskLevel), SecurityInfo.RiskScore,
             SecurityInfo.UID, SecurityInfo.EUID,
             Ord(SecurityInfo.IsRoot), Ord(SecurityInfo.IsSUID), Ord(SecurityInfo.IsSGID),
             SecurityInfo.ExecutablePath,
             Ord(SecurityInfo.ExecutableDeleted), Ord(SecurityInfo.SuspiciousLocation),
             Ord(SecurityInfo.SuspiciousName), Ord(SecurityInfo.Capabilities.HasElevatedCaps),
             PortsList,
             Ord(SecurityInfo.HasPrivilegedPort), Ord(SecurityInfo.HasRawSocket),
             SecurityInfo.ThreatDescription]));
        end;
      end;
      
      Result := True;
    finally
      CloseFile(F);
    end;
  except
    Result := False;
  end;
end;

function TTaskManager.ExportSecurityEventsToCSV(const FileName: string): Boolean;
var
  F: TextFile;
  i: Integer;
  Events: TSecurityEventArray;
begin
  Result := False;
  Events := GetSecurityEvents;
  
  try
    AssignFile(F, FileName);
    Rewrite(F);
    try
      WriteLn(F, 'Timestamp,PID,ProcessName,EventType,RiskLevel,Description');
      
      for i := 0 to Length(Events) - 1 do
      begin
        WriteLn(F, Format('"%s",%d,"%s","%s",%d,"%s"',
          [FormatDateTime('yyyy-mm-dd hh:nn:ss', Events[i].Timestamp),
           Events[i].PID,
           Events[i].ProcessName,
           ThreatTypeToString(Events[i].EventType),
           Ord(Events[i].RiskLevel),
           Events[i].Description]));
      end;
      
      Result := True;
    finally
      CloseFile(F);
    end;
  except
    Result := False;
  end;
end;

function TTaskManager.ExportSecurityReportToText(const FileName: string): Boolean;
var
  F: TextFile;
  i: Integer;
  TopRisky: TProcessInfoArray;
  RootProcs, SuidProcs, SuspProcs: TProcessInfoArray;
begin
  Result := False;
  try
    AssignFile(F, FileName);
    Rewrite(F);
    try
      WriteLn(F, '==================================================');
      WriteLn(F, '        SECURITY ANALYSIS REPORT');
      WriteLn(F, '==================================================');
      WriteLn(F, Format('Generated: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]));
      WriteLn(F, Format('Total Processes Analyzed: %d', [Length(FProcessList)]));
      WriteLn(F, '');
      
      WriteLn(F, '--- EXECUTIVE SUMMARY ---');
      WriteLn(F, SystemSecurityStatsToString);
      WriteLn(F, '');
      
      WriteLn(F, '--- TOP 5 RISKY PROCESSES ---');
      TopRisky := GetTopRiskyProcesses(5);
      for i := 0 to Length(TopRisky) - 1 do
      begin
        WriteLn(F, ProcessSecurityDetailsToString(TopRisky[i]));
        if i < Length(TopRisky) - 1 then
          WriteLn(F, '');
      end;
      WriteLn(F, '');
      
      WriteLn(F, '--- ROOT PROCESSES ---');
      RootProcs := FilterRootProcesses;
      WriteLn(F, Format('Total: %d processes', [Length(RootProcs)]));
      for i := 0 to Min(9, Length(RootProcs) - 1) do
        WriteLn(F, Format('  PID %d: %s', [RootProcs[i].PID, RootProcs[i].Name]));
      WriteLn(F, '');
      
      WriteLn(F, '--- SUID PROCESSES ---');
      SuidProcs := FilterSuidProcesses;
      WriteLn(F, Format('Total: %d processes', [Length(SuidProcs)]));
      for i := 0 to Min(9, Length(SuidProcs) - 1) do
        WriteLn(F, Format('  PID %d: %s (UID: %d -> EUID: %d)', 
          [SuidProcs[i].PID, SuidProcs[i].Name, 
           SuidProcs[i].SecurityInfo.UID, SuidProcs[i].SecurityInfo.EUID]));
      WriteLn(F, '');
      
      WriteLn(F, '--- SUSPICIOUS PROCESSES ---');
      SuspProcs := FilterSuspiciousProcesses;
      WriteLn(F, Format('Total: %d processes', [Length(SuspProcs)]));
      for i := 0 to Min(9, Length(SuspProcs) - 1) do
      begin
        WriteLn(F, Format('  PID %d: %s', [SuspProcs[i].PID, SuspProcs[i].Name]));
        WriteLn(F, Format('    Threats: %s', [SuspProcs[i].SecurityInfo.ThreatDescription]));
        if SuspProcs[i].SecurityInfo.ExecutablePath <> '' then
          WriteLn(F, Format('    Path: %s', [SuspProcs[i].SecurityInfo.ExecutablePath]));
      end;
      WriteLn(F, '');
      
      WriteLn(F, '==================================================');
      WriteLn(F, '              END OF REPORT');
      WriteLn(F, '==================================================');
      
      Result := True;
    finally
      CloseFile(F);
    end;
  except
    Result := False;
  end;
end;

function TTaskManager.ExportMemoryBreakdownToCSV(const FileName: string): Boolean;
var
  F: TextFile;
  i: Integer;
begin
  Result := False;
  try
    AssignFile(F, FileName);
    Rewrite(F);
    try
      WriteLn(F, 'PID,Name,VmSize,VmRSS,VmPeak,VmHWM,RssAnon,RssFile,RssShmem,VmData,VmStk,VmExe,VmLib,VmPTE,VmSwap,VmLock,PrivateMemory,SharedMemory,TotalRSS,SwapUsage');
      
      for i := 0 to Length(FProcessList) - 1 do
      begin
        with FProcessList[i] do
        begin
          WriteLn(F, Format('%d,"%s",%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d',
            [PID, Name,
             MemBreakdown.VmSize, MemBreakdown.VmRSS, MemBreakdown.VmPeak, MemBreakdown.VmHWM,
             MemBreakdown.RssAnon, MemBreakdown.RssFile, MemBreakdown.RssShmem,
             MemBreakdown.VmData, MemBreakdown.VmStk, MemBreakdown.VmExe, MemBreakdown.VmLib,
             MemBreakdown.VmPTE, MemBreakdown.VmSwap, MemBreakdown.VmLock,
             MemBreakdown.PrivateMemory, MemBreakdown.SharedMemory, MemBreakdown.TotalRSS, MemBreakdown.SwapUsage]));
        end;
      end;
      
      Result := True;
    finally
      CloseFile(F);
    end;
  except
    Result := False;
  end;
end;

function TTaskManager.ExportMemoryBreakdownToText(const FileName: string): Boolean;
var
  F: TextFile;
  i: Integer;
begin
  Result := False;
  try
    AssignFile(F, FileName);
    Rewrite(F);
    try
      WriteLn(F, '========== MEMORY BREAKDOWN EXPORT ==========');
      WriteLn(F, Format('Generated: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]));
      WriteLn(F, Format('Total Processes: %d', [Length(FProcessList)]));
      WriteLn(F, '');
      
      WriteLn(F, '--- System Memory Breakdown ---');
      with FSystemStats.MemBreakdown do
      begin
        WriteLn(F, Format('Total Private Memory:    %s', [FormatBytes(TotalPrivateMemoryKB * 1024)]));
        WriteLn(F, Format('Total Shared Memory:     %s', [FormatBytes(TotalSharedMemoryKB * 1024)]));
        WriteLn(F, Format('Total Swap Usage:        %s', [FormatBytes(TotalSwapUsageKB * 1024)]));
        WriteLn(F, Format('Processes Using Swap:    %d', [ProcessesUsingSwap]));
      end;
      WriteLn(F, '');
      
      WriteLn(F, '--- Per-Process Memory Details ---');
      for i := 0 to Length(FProcessList) - 1 do
      begin
        WriteLn(F, '');
        WriteLn(F, ProcessMemoryDetailsToString(FProcessList[i]));
      end;
      
      WriteLn(F, '');
      WriteLn(F, '==============================================');
      Result := True;
    finally
      CloseFile(F);
    end;
  except
    Result := False;
  end;
end;

procedure TTaskManager.SelfTest;
var
  i: Integer;
  TestProc: TProcessInfo;
  FilteredProcs: TProcessInfoArray;
begin
  WriteLn('========================================');
  WriteLn('   TASK MANAGER - COMPREHENSIVE SELF TEST');
  WriteLn('   WITH SECURITY ANALYSIS');
  WriteLn('========================================');
  WriteLn('');
  
  WriteLn('[TEST 1] Refreshing process list...');
  i := RefreshProcessList;
  WriteLn(Format('Found %d processes', [i]));
  WriteLn('');
  
  WriteLn('[TEST 2] System statistics:');
  PrintSystemStats;
  WriteLn('');
  
  WriteLn('[TEST 3] Security Statistics:');
  PrintSecurityStatistics;
  WriteLn('');
  
  WriteLn('[TEST 4] Top 5 risky processes:');
  FilteredProcs := GetTopRiskyProcesses(5);
  for i := 0 to Length(FilteredProcs) - 1 do
    WriteLn(Format('  %s', [SecurityInfoToString(FilteredProcs[i].SecurityInfo)]));
  WriteLn('');
  
  WriteLn('[TEST 5] Root processes:');
  FilteredProcs := FilterRootProcesses;
  WriteLn(Format('Found %d root processes', [Length(FilteredProcs)]));
  for i := 0 to Min(4, Length(FilteredProcs) - 1) do
    WriteLn(Format('  PID %d: %s', [FilteredProcs[i].PID, FilteredProcs[i].Name]));
  WriteLn('');
  
  WriteLn('[TEST 6] SUID processes:');
  FilteredProcs := FilterSuidProcesses;
  WriteLn(Format('Found %d SUID processes', [Length(FilteredProcs)]));
  for i := 0 to Min(4, Length(FilteredProcs) - 1) do
    WriteLn(Format('  PID %d: %s (UID: %d -> EUID: %d)', 
      [FilteredProcs[i].PID, FilteredProcs[i].Name,
       FilteredProcs[i].SecurityInfo.UID, FilteredProcs[i].SecurityInfo.EUID]));
  WriteLn('');
  
  WriteLn('[TEST 7] Suspicious processes:');
  FilteredProcs := FilterSuspiciousProcesses;
  WriteLn(Format('Found %d suspicious processes', [Length(FilteredProcs)]));
  for i := 0 to Min(4, Length(FilteredProcs) - 1) do
    WriteLn(Format('  PID %d: %s - %s', 
      [FilteredProcs[i].PID, FilteredProcs[i].Name,
       FilteredProcs[i].SecurityInfo.ThreatDescription]));
  WriteLn('');
  
  WriteLn('[TEST 8] Processes with privileged ports:');
  FilteredProcs := FilterPrivilegedPorts;
  WriteLn(Format('Found %d processes with privileged ports', [Length(FilteredProcs)]));
  for i := 0 to Min(4, Length(FilteredProcs) - 1) do
    WriteLn(Format('  PID %d: %s', [FilteredProcs[i].PID, FilteredProcs[i].Name]));
  WriteLn('');
  
  WriteLn('[TEST 9] Processes with elevated capabilities:');
  FilteredProcs := FilterElevatedCapabilities;
  WriteLn(Format('Found %d processes with elevated capabilities', [Length(FilteredProcs)]));
  for i := 0 to Min(4, Length(FilteredProcs) - 1) do
    WriteLn(Format('  PID %d: %s', [FilteredProcs[i].PID, FilteredProcs[i].Name]));
  WriteLn('');
  
  WriteLn('[TEST 10] Filter by risk level (Medium or higher):');
  FilteredProcs := FilterByRiskLevel(srlMediumRisk);
  WriteLn(Format('Found %d processes', [Length(FilteredProcs)]));
  for i := 0 to Min(4, Length(FilteredProcs) - 1) do
    WriteLn(Format('  PID %d: %s - Risk: %s (Score: %d)', 
      [FilteredProcs[i].PID, FilteredProcs[i].Name,
       RiskLevelToString(FilteredProcs[i].SecurityInfo.RiskLevel),
       FilteredProcs[i].SecurityInfo.RiskScore]));
  WriteLn('');
  
  WriteLn('[TEST 11] Deleted executables:');
  FilteredProcs := FilterDeletedExecutables;
  WriteLn(Format('Found %d processes with deleted executables', [Length(FilteredProcs)]));
  for i := 0 to Min(4, Length(FilteredProcs) - 1) do
    WriteLn(Format('  PID %d: %s - Path: %s', 
      [FilteredProcs[i].PID, FilteredProcs[i].Name,
       FilteredProcs[i].SecurityInfo.ExecutablePath]));
  WriteLn('');
  
  WriteLn('[TEST 12] Filter by threat type (Root Privilege):');
  FilteredProcs := FilterByThreatType(sttRootPrivilege);
  WriteLn(Format('Found %d processes', [Length(FilteredProcs)]));
  WriteLn('');
  
  WriteLn('[TEST 13] Sorting by security risk (descending):');
  SortProcesses(scSecurityRisk, True);
  for i := 0 to Min(4, Length(FProcessList) - 1) do
    WriteLn(Format('  %s', [SecurityInfoToString(FProcessList[i].SecurityInfo)]));
  WriteLn('');
  
  WriteLn('[TEST 14] Detailed security analysis for init (PID 1):');
  TestProc := GetProcessByPID(1);
  if TestProc.PID = 1 then
    WriteLn(ProcessSecurityDetailsToString(TestProc))
  else
    WriteLn('Init process not found');
  WriteLn('');
  
  WriteLn('[TEST 15] Security events log:');
  PrintSecurityEvents(10);
  WriteLn('');
  
  WriteLn('[TEST 16] Exporting security analysis to CSV:');
  if ExportSecurityAnalysisToCSV('security_analysis.csv') then
    WriteLn('Successfully exported to security_analysis.csv')
  else
    WriteLn('Failed to export CSV');
  WriteLn('');
  
  WriteLn('[TEST 17] Exporting security events to CSV:');
  if ExportSecurityEventsToCSV('security_events.csv') then
    WriteLn('Successfully exported to security_events.csv')
  else
    WriteLn('Failed to export CSV');
  WriteLn('');
  
  WriteLn('[TEST 18] Exporting security report to text:');
  if ExportSecurityReportToText('security_report.txt') then
    WriteLn('Successfully exported to security_report.txt')
  else
    WriteLn('Failed to export text report');
  WriteLn('');
  
  WriteLn('[TEST 19] Security analysis overview:');
  PrintSecurityAnalysis;
  WriteLn('');
  
  WriteLn('[TEST 20] Security threats detected:');
  PrintSecurityThreats;
  WriteLn('');
  
  WriteLn('[TEST 21] Detailed security analysis (top 3):');
  PrintDetailedSecurityAnalysis(3);
  WriteLn('');
  
  WriteLn('[TEST 22] Top risky processes (5):');
  PrintRiskyProcesses(5);
  WriteLn('');
  
  WriteLn('========================================');
  WriteLn('   SELF TEST COMPLETED SUCCESSFULLY');
  WriteLn('   ALL SECURITY FEATURES TESTED');
  WriteLn('========================================');
end;

var
  TaskMgr: TTaskManager;
  
begin
  WriteLn('Task Manager for Linux - Free Pascal Implementation');
  WriteLn('With Comprehensive Security Analysis');
  WriteLn('====================================================');
  WriteLn('');
  
  TaskMgr := TTaskManager.Create;
  try
    TaskMgr.SelfTest;
  finally
    TaskMgr.Free;
  end;
  
  WriteLn('');
  WriteLn('Task Manager execution completed successfully.');
end.
