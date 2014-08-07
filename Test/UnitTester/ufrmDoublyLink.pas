unit ufrmDoublyLink;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, iocpTcpServer, iocpTask, BaseQueue,
  iocpEngine,
  iocpLogger,
  uThreadWorker;

type
  TfrmDoublyLink = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    btnSingleProduce: TButton;
    btnSingleConsum: TButton;
    btnDoubly: TButton;
    procedure btnDoublyClick(Sender: TObject);
    procedure btnSingleConsumClick(Sender: TObject);
    procedure btnSingleProduceClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FDoublyLink:TContextDoublyLinked;
    FIocpRequestSingleLink: TIocpRequestSingleLink;

    FPool:TBaseQueue;
    FCounter:Integer;

    FProduceCounter:Integer;
    FConsumeCounter:Integer;

    procedure OnConsumersDone(pvSender:tObject);
    procedure OnProducersDone(pvSender:tObject);
  public
    destructor Destroy; override;
    procedure OnPopWork;
    { Public declarations }
    procedure OnPushWork;
    { Public declarations }

    procedure onProduce(pvWoker:TThreadWorker);

    procedure onConsume(pvWoker:TThreadWorker);

    procedure onSingleProduce(pvWoker:TThreadWorker);

    procedure onSingleConsume(pvWoker:TThreadWorker);
  end;

var
  frmDoublyLink:TfrmDoublyLink;

implementation


{$R *.dfm}

destructor TfrmDoublyLink.Destroy;
begin
  FDoublyLink.Free;

  FPool.Free;
  FIocpRequestSingleLink.Free;
  inherited Destroy;
end;

procedure TfrmDoublyLink.btnDoublyClick(Sender: TObject);
var
  i:Integer;
begin
  FConsumeCounter := 0;
  FProduceCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.OnConsume := OnConsume;
  workerMgr.OnProduce := nil;
  workerMgr.OnConsumeWorkersDone := self.OnConsumersDone;
  workerMgr.OnProduceWorkersDone := nil;
  workerMgr.start();
end;

procedure TfrmDoublyLink.btnSingleConsumClick(Sender: TObject);
var
  i:Integer;
begin
  FConsumeCounter := 0;
  FProduceCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.OnConsume := onSingleConsume;
  workerMgr.OnProduce := nil;
  workerMgr.OnConsumeWorkersDone := self.OnConsumersDone;
  workerMgr.OnProduceWorkersDone := nil;
  workerMgr.start();

end;

procedure TfrmDoublyLink.btnSingleProduceClick(Sender: TObject);
var
  i:Integer;
begin
  FConsumeCounter := 0;
  FProduceCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.OnConsume := onSingleConsume;
  workerMgr.OnProduce := onSingleProduce;
  workerMgr.OnConsumeWorkersDone := OnConsumersDone;
  workerMgr.OnProduceWorkersDone := OnProducersDone;
  workerMgr.start(7, 3);

end;

procedure TfrmDoublyLink.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 10000 do
  begin
    iocpTaskManager.PostATask(OnPushWork);
  end;

  Sleep(10);

  for i := 0 to 10000 do
  begin
    iocpTaskManager.PostATask(OnPopWork);
  end;

end;

procedure TfrmDoublyLink.Button2Click(Sender: TObject);
var
  i:Integer;
begin
  FProduceCounter := 0;
  FConsumeCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.OnConsume := onConsume;
  workerMgr.OnProduce := onProduce;
  workerMgr.OnConsumeWorkersDone := self.OnConsumersDone;
  workerMgr.OnProduceWorkersDone := self.OnProducersDone;
  workerMgr.start();
end;

procedure TfrmDoublyLink.FormCreate(Sender: TObject);
begin
  uiLogger.setLogLines(Memo1.Lines);
  FDoublyLink := TContextDoublyLinked.Create;
  FIocpRequestSingleLink := TIocpRequestSingleLink.Create(100000);
  FPool := TBaseQueue.Create;
end;

procedure TfrmDoublyLink.onConsume(pvWoker: TThreadWorker);
var
  lvContext:TIocpClientContext;
  i:Integer;
begin
  i:=0;
  while not pvWoker.IsTerminated do
  begin
    lvContext := TIocpClientContext(FPool.Pop);
    if lvContext <> nil then
    begin
      FDoublyLink.remove(lvContext);
      lvContext.Free;
      inc(i);
      InterlockedIncrement(FConsumeCounter);
    end else
    begin
      Break;
    end;
  end;
end;

procedure TfrmDoublyLink.OnConsumersDone(pvSender: tObject);
begin
  uiLogger.logMessage('consume counter:%d', [FConsumeCounter]);
end;

procedure TfrmDoublyLink.OnPopWork;
var
  lvContext:TIocpClientContext;
begin
  lvContext := TIocpClientContext(FPool.Pop);
  if lvContext <> nil then
  begin
    FDoublyLink.remove(lvContext);
    lvContext.Free;
  end;
end;

procedure TfrmDoublyLink.onProduce(pvWoker: TThreadWorker);
var
  lvContext:TIocpClientContext;
  i:Integer;
begin
  for i := 1 to 100000 do
  begin
    lvContext := TIocpClientContext.Create;
    FDoublyLink.add(lvContext);
    FPool.Push(lvContext);
    InterlockedIncrement(FProduceCounter);
  end;
end;

procedure TfrmDoublyLink.OnProducersDone(pvSender: tObject);
begin
  uiLogger.logMessage('produce counter:%d', [FProduceCounter]);

end;

procedure TfrmDoublyLink.OnPushWork;
var
  lvContext:TIocpClientContext;
begin
  lvContext := TIocpClientContext.Create;
  FDoublyLink.add(lvContext);
  FPool.Push(lvContext);
end;

procedure TfrmDoublyLink.onSingleConsume(pvWoker: TThreadWorker);
var
  lvRequest:TIocpSendRequest;
  i:Integer;
begin
  i:=0;
  while not pvWoker.IsTerminated do
  begin
    lvRequest := TIocpSendRequest(FIocpRequestSingleLink.Pop);
    if lvRequest <> nil then
    begin
      lvRequest.Free;
      inc(i);
      InterlockedIncrement(FConsumeCounter);
    end else
    begin
      Break;
    end;
  end;
end;

procedure TfrmDoublyLink.onSingleProduce(pvWoker: TThreadWorker);
var
  lvRequest:TIocpSendRequest;
  i:Integer;
begin
  for i := 1 to 100000 do
  begin
    lvRequest := TIocpSendRequest.Create;
    if FIocpRequestSingleLink.Push(lvRequest) then
    begin
      InterlockedIncrement(FProduceCounter);
    end;
  end;

end;

end.
