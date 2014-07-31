unit uThreadWorker;

interface

uses
  Classes, iocpTask, SysUtils, Windows, BaseQueue;




type
  TThreadWorker = class;
  TWorkerManager = class;

  TOnTaskWork = procedure(pvWorker: TThreadWorker) of object;

  TThreadWorker = class(TThread)
  private
    FOwner:TWorkerManager;
    FData: Pointer;
    FOnWorkStart: TNotifyEvent;
    FOnWorkDone: TNotifyEvent;
    FWorkProc: TOnTaskWork;
    function GetIsTerminated: Boolean;
    constructor Create(CreateSuspended: Boolean; pvOwner: TWorkerManager;
        AWorkProc: TOnTaskWork; AData: Pointer = nil); overload;
  public
    property Data: Pointer read FData;
    property IsTerminated: Boolean read GetIsTerminated;
    property OnWorkStart: TNotifyEvent read FOnWorkStart write FOnWorkStart;
    property OnWorkDone: TNotifyEvent read FOnWorkDone write FOnWorkDone;


    constructor Create(CreateSuspended: Boolean; AWorkProc: TOnTaskWork; AData:
        Pointer = nil); overload;
    constructor Create(AWorkProc: TOnTaskWork; AData: Pointer = nil);overload;
    procedure Execute();override;


  end;

  TWorkerManager = class(TComponent)
  private
    FConsumerCounter: Integer;
    FProducerCounter: Integer;

    FOnConsume: TOnTaskWork;
    FOnConsumeWorkersDone: TNotifyEvent;
    FOnProduce: TOnTaskWork;
    FOnProduceWorkersDone: TNotifyEvent;

    procedure syncConsumersDone;
    procedure syncProducersDone;

    procedure notifyConsumerDone(pvWorker: TObject);
    procedure notifyProducerDone(pvWorker: TObject);
  public





    constructor Create(AOwner: TComponent); override;
    procedure checkIsDone;
    procedure start(pvProduceNum: Integer = 18; pvConsumeNum: Integer = 10;
        pvProduceData: Pointer = nil; pvConsumeData: Pointer = nil);

    property OnConsume: TOnTaskWork read FOnConsume write FOnConsume;
    property OnProduce: TOnTaskWork read FOnProduce write FOnProduce;
    property OnConsumeWorkersDone: TNotifyEvent read FOnConsumeWorkersDone write
        FOnConsumeWorkersDone;
    property OnProduceWorkersDone: TNotifyEvent read FOnProduceWorkersDone write
        FOnProduceWorkersDone;
  end;

var
  workerMgr:TWorkerManager;


implementation

constructor TThreadWorker.Create(AWorkProc: TOnTaskWork; AData: Pointer = nil);
begin
  inherited Create(False);

  FreeOnTerminate := true;
  FData := AData;
  FWorkProc := AWorkProc;
end;

{ TThreadWorker }

constructor TThreadWorker.Create(CreateSuspended: Boolean; AWorkProc:
    TOnTaskWork; AData: Pointer = nil);
begin
  inherited Create(CreateSuspended);

  FreeOnTerminate := true;
  FData := AData;
  FWorkProc := AWorkProc;
end;

constructor TThreadWorker.Create(CreateSuspended: Boolean; pvOwner:
    TWorkerManager; AWorkProc: TOnTaskWork; AData: Pointer = nil);
begin
  inherited Create(True);
  FOwner := pvOwner;
  FreeOnTerminate := true;
  FData := AData;
  FWorkProc := AWorkProc;
end;

procedure TThreadWorker.Execute;
begin
  if Assigned(FOnWorkStart) then FOnWorkStart(Self);

  try
    if Assigned(FWorkProc) then
    begin
      FWorkProc(Self);
    end;
  finally
    if Assigned(FOnWorkDone) then FOnWorkDone(Self);
  end;
end;

function TThreadWorker.GetIsTerminated: Boolean;
begin
  Result := Terminated;
end;

procedure TWorkerManager.checkIsDone;
begin
  if (FConsumerCounter <> 0) then
  begin
    raise exception.Create('consumer still working');
  end;

  if (FProducerCounter <> 0) then
  begin
    raise exception.Create('producer still working');
  end;
end;

constructor TWorkerManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConsumerCounter := 0;
  FProducerCounter := 0;
end;

procedure TWorkerManager.notifyConsumerDone(pvWorker: TObject);
begin
  InterlockedDecrement(FConsumerCounter);
  if FConsumerCounter = 0 then
  begin
    TThread.Synchronize(nil, syncConsumersDone);
  end;
end;

procedure TWorkerManager.notifyProducerDone(pvWorker: TObject);
begin
  InterlockedDecrement(FProducerCounter);
  if FProducerCounter = 0 then
  begin
    TThread.Synchronize(nil, syncProducersDone);
  end;
end;

{ TWorkerManager }

procedure TWorkerManager.start(pvProduceNum: Integer = 18; pvConsumeNum:
    Integer = 10; pvProduceData: Pointer = nil; pvConsumeData: Pointer = nil);
var
  i: Integer;
  lvWorker:TThreadWorker;
begin
  FConsumerCounter := pvConsumeNum;
  FProducerCounter := pvProduceNum;
  for i := 0 to pvProduceNum -1 do
  begin
    lvWorker := TThreadWorker.Create(True, Self, FOnProduce, pvProduceData);
    lvWorker.OnWorkDone := notifyProducerDone;
    lvWorker.Resume;
  end;




  for i := 0 to pvConsumeNum -1 do
  begin
    lvWorker := TThreadWorker.Create(True, Self, FOnConsume, pvConsumeData);
    lvWorker.OnWorkDone := notifyConsumerDone;
    lvWorker.Resume;
  end;



end;

procedure TWorkerManager.syncConsumersDone;
begin
  if Assigned(FOnConsumeWorkersDone) then FOnConsumeWorkersDone(Self);
end;

procedure TWorkerManager.syncProducersDone;
begin
 if Assigned(FOnProduceWorkersDone) then FOnProduceWorkersDone(Self);
end;

initialization
  workerMgr := TWorkerManager.Create(nil);

finalization
  workerMgr.Free;

end.
