(*
 *	 Unit owner: D10.Mofen
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *   v0.0.2(2014-09-06 22:06:18)
 *     + warning, hint
 *
 *	 v0.0.1(2014-08-31 12:40:18)
 *     + first release
 *)

unit safeLogger;

interface

uses
  Classes, BaseQueue, SysUtils, SyncObjs{$IFDEF MSWINDOWS}, Windows, Messages {$ENDIF};

type

  TLogLevel=(lgvError, lgvWarning, lgvHint, lgvMessage, lgvDebug);

const
  TLogLevelCaption: array [TLogLevel] of string = ('error', 'warning', 'hint', 'message', 'debug');

type
  TSafeLogger = class;
  TSyncMainThreadType = (rtSync{$IFDEF MSWINDOWS}, rtPostMessage {$ENDIF});

  TLogDataObject = class(TObject)
  public
    FThreadID:Cardinal;
    FTime:TDateTime;
    FLogLevel:TLogLevel;
    FMsg:string;
    FMsgType:string;
  end;

  TBaseAppender = class(TObject)
  protected
    FOwner:TSafeLogger;
  protected
    procedure AppendLog(pvData:TLogDataObject); virtual; abstract;
  end;

  TConsoleAppender = class(TBaseAppender)
  protected
    procedure AppendLog(pvData:TLogDataObject); override;
  public
  end;

  TStringsAppender = class(TBaseAppender)
  private
    FStrings: TStrings;
  protected
    procedure AppendLog(pvData:TLogDataObject); override;
  public
    constructor Create(AStrings: TStrings);
  end;

  TLogFileAppender = class(TBaseAppender)
  private
    FAddThreadINfo: Boolean;
    FBasePath: string;
    FLogFile: TextFile;
    FInitialized: Boolean;
    procedure checkInitialized;
    function openLogFile(pvPre: String = ''): Boolean;
  protected
    procedure AppendLog(pvData:TLogDataObject); override;
  public
    constructor Create(pvAddThreadINfo: Boolean);
    property AddThreadINfo: Boolean read FAddThreadINfo write FAddThreadINfo;
  end;


  TLogWorker = class(TThread)
  private
    {$IFDEF MSWINDOWS}
    FMessageEvent: TEvent;
    {$ENDIF}
    FSafeLogger: TSafeLogger;
    FNotify: TEvent;
    // temp for sync method
    FTempLogData: TLogDataObject;
    procedure ExecuteLogData(const pvData:TLogDataObject);
    procedure InnerSyncLogData;
  public
    constructor Create(ASafeLogger: TSafeLogger);
    destructor Destroy; override;
    procedure Execute; override;
  end;


  TSafeLogger = class(TObject)
  private
    FWorkerAlive:Boolean;
    
    FLogWorker:TLogWorker;
    FDataQueue: TBaseQueue;
    FOwnsAppender:Boolean;

    FAppender: TBaseAppender;
    FAppendInMainThread: Boolean;

    FSyncMainThreadType: TSyncMainThreadType;

    procedure ExecuteLogData(const pvData:TLogDataObject);
  private
    FEnable: Boolean;
    FWorkerCounter:Integer;
    FErrorCounter: Integer;
    FPostCounter: Integer;
    FResponseCounter: Integer;
    procedure incErrorCounter;


    procedure incResponseCounter;
    /// <summary>
    ///   check worker thread is alive
    /// </summary>
    function workersIsAlive(const pvWorker: TLogWorker): Boolean;

    procedure checkForWorker;
    procedure stopWorker;
  private
  {$IFDEF MSWINDOWS}
    FMessageHandle: HWND;
    procedure DoMainThreadWork(var AMsg: TMessage);
  {$ENDIF}
    procedure incWorkerCount;
    procedure decWorker(pvWorker: TLogWorker);

  public
    constructor Create;
    destructor Destroy; override;

    procedure start;

    /// <summary>
    ///   task current info
    /// </summary>
    function getStateINfo: String;

    procedure setAppender(pvAppender: TBaseAppender; pvOwnsAppender: Boolean =
        true);

    procedure logMessage(pvMsg: string; pvMsgType: string = ''; pvLevel: TLogLevel
        = lgvMessage); overload;
    procedure logMessage(pvMsg: string; const args: array of const; pvMsgType:
        string = ''; pvLevel: TLogLevel = lgvMessage); overload;

    property SyncMainThreadType: TSyncMainThreadType read FSyncMainThreadType write
        FSyncMainThreadType;
    property AppendInMainThread: Boolean read FAppendInMainThread write
        FAppendInMainThread;

    property Enable: Boolean read FEnable write FEnable;


  end;

var
  sfLogger:TSafeLogger;

implementation




var
  __dataObjectPool:TBaseQueue;

{$IFDEF MSWINDOWS}
const
  WM_SYNC_METHOD = WM_USER + 1;

{$ENDIF}

/// compare target, cmp_val same set target = new_val
/// return old value
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean): Boolean; overload;
asm
{$ifdef win32}
  lock cmpxchg [ecx], dl
{$else}
.noframe
  mov rax, rcx
  lock cmpxchg [r8], dl
{$endif}
end;

procedure TSafeLogger.checkForWorker;
begin
  if lock_cmp_exchange(False, True, FWorkerAlive) = False then
  begin
    if FLogWorker = nil then
    begin
      FLogWorker := TLogWorker.Create(Self);
    {$IFDEF UNICODE}
      FLogWorker.Start;
    {$ELSE}
      FLogWorker.Resume;
    {$ENDIF}
    end;
  end;
  if FLogWorker <> nil then
  begin
    FLogWorker.FNotify.SetEvent;
  end;
end;

constructor TSafeLogger.Create;
begin
  inherited Create;
  FWorkerAlive := False;
  FEnable := true;
  FSyncMainThreadType := rtSync;
{$IFDEF MSWINDOWS}
  FSyncMainThreadType := rtPostMessage;
  FMessageHandle := AllocateHWnd(DoMainThreadWork);
{$ENDIF}
  FDataQueue := TBaseQueue.Create();
  FAppender := nil;
  FOwnsAppender := false;
  FWorkerCounter := 0;
end;

destructor TSafeLogger.Destroy;
begin
  FEnable := false;
  
  stopWorker;

  FDataQueue.FreeDataObject;
  FreeAndNil(FDataQueue);
  if FOwnsAppender then
  begin
    if FAppender <> nil then
    begin
      FAppender.Free;
      FAppender := nil;
    end;
  end;
{$IFDEF MSWINDOWS}
  DeallocateHWnd(FMessageHandle);
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF MSWINDOWS}
procedure TSafeLogger.DoMainThreadWork(var AMsg: TMessage);
begin
  if AMsg.Msg = WM_SYNC_METHOD then
  begin
    try
      if not FEnable then Exit;
      ExecuteLogData(TLogDataObject(AMsg.WParam));
    finally
      if AMsg.LPARAM <> 0 then
        TEvent(AMsg.LPARAM).SetEvent;
    end;
  end else
    AMsg.Result := DefWindowProc(FMessageHandle, AMsg.Msg, AMsg.WPARAM, AMsg.LPARAM);
end;
{$ENDIF}

procedure TSafeLogger.ExecuteLogData(const pvData:TLogDataObject);
begin
  incResponseCounter;
  if FAppender = nil then
  begin
    incErrorCounter;
  end else
  begin
    FAppender.AppendLog(pvData);
  end;

end;

procedure TSafeLogger.incErrorCounter;
begin
  InterlockedIncrement(FErrorCounter);
end;

procedure TSafeLogger.incWorkerCount;
begin
  InterlockedIncrement(FWorkerCounter);
end;

procedure TSafeLogger.decWorker(pvWorker: TLogWorker);
begin
  InterlockedDecrement(FWorkerCounter);
  FWorkerAlive := false;
  FLogWorker := nil;
end;

function TSafeLogger.getStateINfo: String;
var
  lvDebugINfo:TStrings;
begin
  lvDebugINfo := TStringList.Create;
  try
    lvDebugINfo.Add(Format('enable:%s, workerAlive:%s', [boolToStr(FEnable, True), boolToStr(FWorkerAlive, True)]));
    lvDebugINfo.Add(Format('post/response/error counter:%d / %d / %d',
       [self.FPostCounter,self.FResponseCounter,self.FErrorCounter]));
    Result := lvDebugINfo.Text;
  finally
    lvDebugINfo.Free;
  end;
end;

procedure TSafeLogger.incResponseCounter;
begin
  InterlockedIncrement(FResponseCounter);
end;

{ TSafeLogger }

procedure TSafeLogger.logMessage(pvMsg: string; pvMsgType: string = '';
    pvLevel: TLogLevel = lgvMessage);
var
  lvPData:TLogDataObject;
begin
  if not FEnable then exit;

  lvPData := __dataObjectPool.Pop;
  if lvPData = nil then lvPData:=TLogDataObject.Create;
{$IFDEF MSWINDOWS}
  lvPData.FThreadID := GetCurrentThreadId;
{$ELSE}
  lvPData.FThreadID := TThread.CurrentThread.ThreadID;
{$ENDIF};
  lvPData.FTime := Now();
  lvPData.FLogLevel := pvLevel;
  lvPData.FMsg := pvMsg;
  lvPData.FMsgType := pvMsgType;
  FDataQueue.Push(lvPData);
  InterlockedIncrement(FPostCounter);
  checkForWorker;
end;

procedure TSafeLogger.logMessage(pvMsg: string; const args: array of const;
    pvMsgType: string = ''; pvLevel: TLogLevel = lgvMessage);
begin
  logMessage(Format(pvMsg, args), pvMsgType, pvLevel);
end;

procedure TSafeLogger.setAppender(pvAppender: TBaseAppender; pvOwnsAppender:
    Boolean = true);
begin
  if (FAppender <> nil) and FOwnsAppender then
  begin
    FAppender.Free;
    FAppender := nil;
  end;

  if pvAppender <> nil then
  begin
    FAppender := pvAppender;
    FOwnsAppender := pvOwnsAppender;
    FAppender.FOwner := Self;
  end;
end;

procedure TSafeLogger.start;
begin
  //nothing to do ...
end;

procedure TSafeLogger.stopWorker;
begin
  if FLogWorker <> nil then
  begin
    FLogWorker := FLogWorker;
    if FLogWorker <> nil then
    begin
      FLogWorker.Terminate;
      FLogWorker.FNotify.SetEvent;
    end;
    while (FWorkerCounter > 0) and workersIsAlive(FLogWorker) do
    begin
      {$IFDEF MSWINDOWS}
      SwitchToThread;
      {$ELSE}
      TThread.Yield;
      {$ENDIF}
    end;
    FLogWorker := nil;
  end;
end;

function TSafeLogger.workersIsAlive(const pvWorker: TLogWorker): Boolean;
var
  lvCode:Cardinal;
begin
  Result := false;
  if (pvWorker <> nil) and (GetExitCodeThread(pvWorker.Handle, lvCode)) then
  begin
    if lvCode=STILL_ACTIVE then
    begin
      Result := true;
    end;
  end;
end;

constructor TLogWorker.Create(ASafeLogger: TSafeLogger);
begin
  inherited Create(True);
  FreeOnTerminate := true;
  FNotify := TEvent.Create(nil,false,false,'');
  FSafeLogger := ASafeLogger;
  FMessageEvent := TEvent.Create(nil, true, False, '');

end;

destructor TLogWorker.Destroy;
begin
  FNotify.Free;
  FMessageEvent.Free;
  inherited Destroy;
end;

procedure TLogWorker.Execute;
var
  lvPData:TLogDataObject;
  lvWaitResult:TWaitResult;
begin
  FSafeLogger.incWorkerCount;
  try
    while not self.Terminated do
    begin
      lvWaitResult := FNotify.WaitFor(1000 * 30);
      if (lvWaitResult=wrSignaled) then
      begin
        while not self.Terminated do
        begin
          lvPData := FSafeLogger.FDataQueue.Pop;
          if lvPData = nil then Break;

          ExecuteLogData(lvPData);

          /// push back to logData pool
          __dataObjectPool.Push(lvPData);
        end;
      end else if lvWaitResult = wrTimeout then
      begin
        Break;
      end;
    end;
  finally
    FSafeLogger.decWorker(Self);
  end;
end;

procedure TLogWorker.ExecuteLogData(const pvData:TLogDataObject);
begin
  if FSafeLogger.FAppendInMainThread then
  begin
    if FSafeLogger.FSyncMainThreadType = rtSync then
    begin
      FTempLogData := pvData;
      Synchronize(InnerSyncLogData);
    end
{$IFDEF MSWINDOWS}
    else if FSafeLogger.FSyncMainThreadType = rtPostMessage then
    begin
      FMessageEvent.ResetEvent;
      if PostMessage(FSafeLogger.FMessageHandle, WM_SYNC_METHOD, WPARAM(pvData), LPARAM(FMessageEvent)) then
      begin
        FMessageEvent.WaitFor(INFINITE);
      end else
      begin
        FSafeLogger.incErrorCounter;
        // log exception
      end;
    end
{$ENDIF}
    ;
  end else
  begin
    FSafeLogger.ExecuteLogData(pvData);
  end;
end;

procedure TLogWorker.InnerSyncLogData;
begin
   FSafeLogger.ExecuteLogData(FTempLogData);
end;

constructor TStringsAppender.Create(AStrings: TStrings);
begin
  inherited Create;
  FStrings := AStrings;
end;

procedure TStringsAppender.AppendLog(pvData:TLogDataObject);
begin
  inherited;
  Assert(FStrings <> nil);
  FStrings.Add(
    Format('%s[%s]:%s',
      [FormatDateTime('yyyy-MM-dd hh:nn:ss.zzz', pvData.FTime)
        , TLogLevelCaption[pvData.FLogLevel]
        , pvData.FMsg
      ]
      ));
end;

procedure TLogFileAppender.AppendLog(pvData: TLogDataObject);
var
  lvMsg:String;
begin
  checkInitialized;
  if OpenLogFile(pvData.FMsgType) then
  begin
    try
      if FAddThreadINfo then
      begin
        lvMsg := Format('%s[%s][PID:%d,ThreadID:%d]:%s',
            [FormatDateTime('hh:nn:ss:zzz', pvData.FTime)
              , TLogLevelCaption[pvData.FLogLevel]
              , GetCurrentProcessID()
              , pvData.FThreadID
              , pvData.FMsg
            ]
            );
      end else
      begin
        lvMsg := Format('%s[%s]:%s',
            [FormatDateTime('hh:nn:ss:zzz', pvData.FTime)
              , TLogLevelCaption[pvData.FLogLevel]
              , pvData.FMsg
            ]
            );
      end;
      writeln(FLogFile, lvMsg);
      flush(FLogFile);
    finally
      CloseFile(FLogFile);
    end;
  end else
  begin
    FOwner.incErrorCounter;
  end;
end;

procedure TLogFileAppender.checkInitialized;
begin
  if FInitialized then exit;
  if not DirectoryExists(FBasePath) then ForceDirectories(FBasePath);
  FInitialized := true;
end;

constructor TLogFileAppender.Create(pvAddThreadINfo: Boolean);
begin
  inherited Create;
  FBasePath :=ExtractFilePath(ParamStr(0)) + 'log';
  FAddThreadINfo := pvAddThreadINfo;
end;

function TLogFileAppender.openLogFile(pvPre: String = ''): Boolean;
var
  lvFileName:String;
begin


  lvFileName :=FBasePath + '\' + pvPre + FormatDateTime('yyyymmddhh', Now()) + '.log';
  try
    AssignFile(FLogFile, lvFileName);
    if (FileExists(lvFileName)) then
      append(FLogFile)
    else
      rewrite(FLogFile);

    Result := true;
  except
    Result := false;
  end;
end;

{ TConsoleAppender }

procedure TConsoleAppender.AppendLog(pvData: TLogDataObject);
begin
  Writeln(
    Format('%s[%s]:%s',
      [FormatDateTime('yyyy-MM-dd hh:nn:ss.zzz', pvData.FTime)
        , TLogLevelCaption[pvData.FLogLevel]
        , pvData.FMsg
      ]
      ));
end;

initialization
  __dataObjectPool := TBaseQueue.Create;
  __dataObjectPool.Name := 'safeLoggerDataPool';
  sfLogger := TSafeLogger.Create();
  sfLogger.setAppender(TLogFileAppender.Create(True));

finalization
  __dataObjectPool.FreeDataObject;
  __dataObjectPool.Free;
  sfLogger.Free;

end.
