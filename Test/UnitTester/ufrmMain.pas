unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, iocpTask, Vcl.StdCtrls,
  iocpEngine, iocpTcpServer, uThreadWorker, BaseQueue, iocpLogger, DoublyLinked;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    btnDoublyLinked: TButton;
    btnDoublyLinkedConsume: TButton;
    procedure btnDoublyLinkedClick(Sender: TObject);
    procedure btnDoublyLinkedConsumeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FTcpSvr:TIocpTcpServer;
    FDoublyLinked: TContextDoublyLinked;

    FDoublyLinked02: TSafeDoublyLinkedList;
    FPool:TBaseQueue;
    FConsumeCounter: Integer;
    FProduceCounter: Integer;
    procedure OnConsumersDone(pvSender:tObject);
    procedure OnProducersDone(pvSender:tObject);



    procedure OnLogTester(pvStr:String);
  public
    destructor Destroy; override;
    procedure onConsume(pvWoker:TThreadWorker);

    procedure onDoublyLinkedConsume(pvWoker:TThreadWorker);
    procedure onDoublyLinkedProduce(pvWoker:TThreadWorker);


    procedure OnLogWorker(pvSender:TThreadWorker);
    procedure onProduce(pvWoker:TThreadWorker);
  end;

var
  frmMain: TfrmMain;

implementation


{$R *.dfm}

var
  gblSN:Integer;

procedure TfrmMain.FormCreate(Sender: TObject);
begin

  FTcpSvr := TIocpTcpServer.Create(Self);
  FTcpSvr.Name := 'tcpServer';
  uiLogger.setLogLines(Memo1.Lines);
  FPool := TBaseQueue.Create;
  FDoublyLinked := TContextDoublyLinked.Create();
  FDoublyLinked02 := TSafeDoublyLinkedList.Create();
end;

destructor TfrmMain.Destroy;
begin
  FreeAndNil(FDoublyLinked02);
  FreeAndNil(FDoublyLinked);
  FTcpSvr.Free;
  FPool.Free;
  inherited Destroy;
end;

procedure TfrmMain.btnDoublyLinkedClick(Sender: TObject);
var
  i:Integer;
begin
  FProduceCounter := 0;
  FConsumeCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.OnConsume := onDoublyLinkedConsume;
  workerMgr.OnProduce := onDoublyLinkedProduce;
  workerMgr.OnConsumeWorkersDone := self.OnConsumersDone;
  workerMgr.OnProduceWorkersDone := self.OnProducersDone;
  workerMgr.start();
end;

procedure TfrmMain.btnDoublyLinkedConsumeClick(Sender: TObject);
var
  i:Integer;
begin
  FProduceCounter := 0;
  FConsumeCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.OnConsume := onDoublyLinkedConsume;
  workerMgr.OnProduce := nil;
  workerMgr.OnConsumeWorkersDone := self.OnConsumersDone;
  workerMgr.OnProduceWorkersDone := nil;
  workerMgr.start(0,1);

end;

procedure TfrmMain.Button1Click(Sender: TObject);
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

procedure TfrmMain.Button2Click(Sender: TObject);
var
  i:Integer;
begin
  FProduceCounter := 0;
  FConsumeCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.OnConsume := onConsume;
  workerMgr.OnProduce := nil;
  workerMgr.OnConsumeWorkersDone := self.OnConsumersDone;
  workerMgr.OnProduceWorkersDone := nil;
  workerMgr.start(0, 1);

end;

procedure TfrmMain.Button3Click(Sender: TObject);
var
  i:Integer;
begin
  gblSN := 0;
  iocpTaskManager.setWorkerCount(1);

  FProduceCounter := 0;
  FConsumeCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.OnConsume := OnLogWorker;
  workerMgr.OnProduce := nil;
  workerMgr.OnConsumeWorkersDone := nil;
  workerMgr.OnProduceWorkersDone := nil;
  workerMgr.start(0, 20);

end;

procedure TfrmMain.onConsume(pvWoker: TThreadWorker);
var
  lvContext:TIocpClientContext;
  i:Integer;
begin
  i := 0;
  while not pvWoker.IsTerminated do
  begin
    lvContext := nil;
    lvContext := TIocpClientContext(FPool.Pop);
//    if i mod 2 = 0 then
//    begin
//      lvContext := TIocpClientContext(FPool.Pop);
//    end else
//    begin
//      lvContext := FDoublyLinked.Pop;
//    end;
    if lvContext = nil then
    begin
      lvContext := nil;
      Break;
    end;
    FDoublyLinked.remove(lvContext);
    FTcpSvr.releaseClientContext(lvContext);
    InterlockedIncrement(FConsumeCounter);
    Inc(i);
  end;
end;

procedure TfrmMain.OnConsumersDone(pvSender: tObject);
begin
  uiLogger.logMessage('consume counter:%d', [FConsumeCounter]);

  uiLogger.logMessage(Format('linked count:%d,%d', [FDoublyLinked.Count, FDoublyLinked02.Size]));
end;

procedure TfrmMain.onDoublyLinkedConsume(pvWoker: TThreadWorker);
var
  lvContext:TBaseDoublyObject;
  i, j:Integer;
begin
  i := 0;
  while not pvWoker.IsTerminated do
  begin
    lvContext := nil;

    //lvContext := TBaseDoublyObject(FPool.Pop);

    if i mod 2 = 0 then
    begin
      lvContext := FDoublyLinked02.PeekLast;
      j := 0;
    end else
    begin
      lvContext := FDoublyLinked02.PeekFirst;
      j := 1;
    end;

    if lvContext = nil then
    begin
      lvContext := nil;
      j := 3;
      Break;
    end;

    if not FDoublyLinked02.isLinked(lvContext) then
    begin
      j := 4;
    end;

    FDoublyLinked02.remove(lvContext);
    lvContext.Free;
    InterlockedIncrement(FConsumeCounter);
    Inc(i);
  end;

  Assert(j = 3);
end;

procedure TfrmMain.onDoublyLinkedProduce(pvWoker: TThreadWorker);
var
  lvContext:TBaseDoublyObject;
  i:Integer;
begin
  for i := 1 to 3000 do
  begin
    lvContext := TBaseDoublyObject.Create;
    FPool.Push(lvContext);
    FDoublyLinked02.addToLast(lvContext);
    InterlockedIncrement(FProduceCounter);
  end;
end;

procedure TfrmMain.OnLogTester(pvStr: String);
begin
  Memo1.Lines.Add(pvStr);
end;

procedure TfrmMain.OnLogWorker(pvSender:TThreadWorker);
var
  lvSN:Integer;
begin
  lvSN := InterlockedIncrement(gblSN);
  iocpTaskManager.PostATask(OnLogTester,IntToStr(lvSN) +  '.1.第一次投递============', true, rtPostMessage);

  lvSN := InterlockedIncrement(gblSN);
  iocpTaskManager.PostATask(OnLogTester, IntToStr(lvSN) +  '.2.第二次', true, rtPostMessage);
end;

procedure TfrmMain.onProduce(pvWoker: TThreadWorker);
var
  lvContext:TIocpClientContext;
  i:Integer;
begin
  for i := 1 to 3000 do
  begin
    lvContext := FTcpSvr.getClientContext;
    FPool.Push(lvContext);
    FDoublyLinked.add(lvContext);
    InterlockedIncrement(FProduceCounter);
  end;
end;

procedure TfrmMain.OnProducersDone(pvSender: tObject);
begin
  uiLogger.logMessage('produce counter:%d', [FProduceCounter]);

end;

end.
