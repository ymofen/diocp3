unit DQueueTask;

interface

uses
  BaseQueue, Classes,
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF}
  SyncObjs;

type
  TDTaskWorker = class;
  TDQueueTaskConsole = class;
  TDataNotifyEvent = procedure(pvTaskConsole: TDQueueTaskConsole; pvData: Pointer)
      of object;

  TDQueueTaskConsole = class(TObject)
  private
    FLocker:TCriticalSection;
    FWorker: TDTaskWorker; 
    FDebugInfo: String;
    FDataQueue: TBaseQueue;
    FEnable: Boolean;
    FOnExecute: TDataNotifyEvent;
    FWorkerAlive: Boolean;
    FCommEvent: TEvent;
    procedure NotifyDestroyWorker;
  protected
    procedure CheckForWorker;
    procedure DoTask(pvData: Pointer); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PostATask(pvData:Pointer);

    procedure DisableWork;
    procedure EnableWork;    
    function StopWorker(pvTimeOut: Cardinal): Boolean; 


    property CommEvent: TEvent read FCommEvent;
    property Enable: Boolean read FEnable;

    property DataQueue: TBaseQueue read FDataQueue;

    property OnExecute: TDataNotifyEvent read FOnExecute write FOnExecute;
  end;


  TDTaskWorker = class(TThread)
  private
    FOwner: TDQueueTaskConsole;
    FNotify: TEvent;
  public
    constructor Create(AOwner: TDQueueTaskConsole);
    destructor Destroy; override;
    procedure Execute; override;
  end;

var
  queueTaskConsole:TDQueueTaskConsole;

implementation

function tick_diff(tick_start, tick_end: Cardinal): Cardinal;
begin
  if tick_end >= tick_start then
    result := tick_end - tick_start
  else
    result := High(Cardinal) - tick_start + tick_end;
end;

function CheckThreadIsAlive(const AThread: TThread): Boolean;
var
  lvCode:Cardinal;
begin
  Result := false;
  if (AThread <> nil) and (GetExitCodeThread(AThread.Handle, lvCode)) then
  begin
    if lvCode=STILL_ACTIVE then
    begin
      Result := true;
    end;
  end;
end;

constructor TDQueueTaskConsole.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create;
  FDataQueue := TBaseQueue.Create;
  FCommEvent := TEvent.Create(nil,false,false,'');
  FEnable := true;                             
end;

destructor TDQueueTaskConsole.Destroy;
begin
  StopWorker(3000);
  FDataQueue.Free;
  FLocker.Free;
  FCommEvent.Free;
  inherited Destroy;
end;

procedure TDQueueTaskConsole.DoTask(pvData: Pointer);
begin
  if Assigned(FOnExecute) then FOnExecute(Self, pvData); 
end;

procedure TDQueueTaskConsole.CheckForWorker;
begin
  FLocker.Enter;
  try
    if FWorker = nil then
    begin
      FWorker := TDTaskWorker.Create(Self);
    {$IF RTLVersion<25}
      FWorker.Resume;
    {$ELSE}
      FWorker.Start;
    {$IFEND}
      Sleep(10);
    end;

    if FWorker <> nil then
    begin
      FWorker.FNotify.SetEvent;
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure TDQueueTaskConsole.NotifyDestroyWorker;
begin
  FLocker.Enter;
  try
    FWorkerAlive := False;
    FWorker := nil;
  finally
    FLocker.Leave;
  end;
end;



procedure TDQueueTaskConsole.PostATask(pvData: Pointer);
begin
  FDataQueue.Push(pvData);
  CheckForWorker;
end;

function TDQueueTaskConsole.StopWorker(pvTimeOut: Cardinal): Boolean;
var
  l:Cardinal;
begin
  Result := true;
  FEnable := false;
  if FWorker <> nil then
  begin
    FWorker.Terminate;
    FWorker.FNotify.SetEvent;
    FCommEvent.SetEvent;


    l := GetTickCount;
    while CheckThreadIsAlive(FWorker) do
    begin
      {$IFDEF MSWINDOWS}
      SwitchToThread;
      {$ELSE}
      TThread.Yield;
      {$ENDIF}

      if tick_diff(l, GetTickCount) > pvTimeOut then
      begin
        Result := false;
        Break;
      end;
    end;
    FWorker := nil;
  end;
end;

procedure TDQueueTaskConsole.DisableWork;
begin
  FEnable := false;
end;

procedure TDQueueTaskConsole.EnableWork;
begin
  FEnable := true;
end;

constructor TDTaskWorker.Create(AOwner: TDQueueTaskConsole);
begin
  inherited Create(True);
  FreeOnTerminate := true;
  FNotify := TEvent.Create(nil,false,false,'');
  FOwner := AOwner;
end;

destructor TDTaskWorker.Destroy;
begin
  FNotify.Free;
  inherited Destroy;
end;

procedure TDTaskWorker.Execute;
var
  lvWaitResult:TWaitResult;
  lvData: Pointer;
begin
  try
    while not self.Terminated do
    begin
      FOwner.FDebugInfo := 'Thread.Execute::FNotify.WaitFor()';
      lvWaitResult := FNotify.WaitFor(1000 * 30);
      if (lvWaitResult=wrSignaled) then
      begin
        FOwner.FDebugInfo := 'Thread.Execute::FNotify.WaitFor(), succ';
        while not self.Terminated do
        begin    
          if not FOwner.FDataQueue.Pop(lvData) then Break;
          try
            FOwner.FDebugInfo := 'Thread.Execute::DoDataRecvdObject';
            FOwner.DoTask(lvData);
          except
            //FOwner.incErrorCounter;
          end;
        end;
      end else if lvWaitResult = wrTimeout then
      begin
        Break;
      end;
    end;
  finally
    FOwner.NotifyDestroyWorker;
  end;

end;

initialization
  queueTaskConsole:= TDQueueTaskConsole.Create;

finalization
  queueTaskConsole.StopWorker(3000);
  queueTaskConsole.Free;
  queueTaskConsole := nil;

end.
