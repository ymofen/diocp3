unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, iocpTask, Vcl.StdCtrls,
  iocpEngine, iocpTcpServer, syncObjs,
  uThreadWorker, BaseQueue, iocpLogger, DoublyLinked, IdGlobal;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    btnDoublyLinked: TButton;
    btnDoublyLinkedConsume: TButton;
    btnBaseQueue: TButton;
    btnBaseQueueConsume: TButton;
    Button4: TButton;
    procedure btnBaseQueueClick(Sender: TObject);
    procedure btnBaseQueueConsumeClick(Sender: TObject);
    procedure btnDoublyLinkedClick(Sender: TObject);
    procedure btnDoublyLinkedConsumeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    FList: TList;
    FCreateList: TList;
    FLocker2: TCriticalSection;
    FLocker: TCriticalSection;
    FTcpSvr:TIocpTcpServer;
    FContextDoublyLinked: TContextDoublyLinked;

    FSafeDoublyLinkedList: TSafeDoublyLinkedList;
    FPool:TBaseQueue;
    FConsumeCounter: Integer;
    FProduceCounter: Integer;
    FFailCounter:Integer;
    procedure OnConsumersDone(pvSender:tObject);
    procedure OnProducersDone(pvSender:tObject);



    procedure OnLogTester(pvStr:String);
  public
    destructor Destroy; override;
    procedure onContextDoublyLinkedConsume(pvWoker:TThreadWorker);
    procedure onContextDoublyLinkedProduce(pvWoker:TThreadWorker);

    procedure onDoublyLinkedConsume(pvWoker:TThreadWorker);
    procedure onDoublyLinkedProduce(pvWoker:TThreadWorker);

    procedure onBaseQueueConsume(pvWoker:TThreadWorker);
    procedure onBaseQueueProduce(pvWoker:TThreadWorker);


    procedure OnLogWorker(pvSender:TThreadWorker);

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
  FContextDoublyLinked := TContextDoublyLinked.Create();
  FSafeDoublyLinkedList := TSafeDoublyLinkedList.Create();
  FList := TList.Create();
  FLocker := TCriticalSection.Create();
  FCreateList := TList.Create();
  FLocker2 := TCriticalSection.Create();
end;

destructor TfrmMain.Destroy;
begin
  FreeAndNil(FLocker2);
  FreeAndNil(FCreateList);
  FreeAndNil(FLocker);
  FreeAndNil(FList);
  FreeAndNil(FSafeDoublyLinkedList);
  FreeAndNil(FContextDoublyLinked);
  FTcpSvr.Free;
  FPool.Free;
  inherited Destroy;
end;

procedure TfrmMain.btnBaseQueueClick(Sender: TObject);
begin
  FProduceCounter := 0;
  FConsumeCounter := 0;
  FFailCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.onConsume := onBaseQueueConsume;
  workerMgr.onProduce := onBaseQueueProduce;
  workerMgr.OnConsumeWorkersDone := self.OnConsumersDone;
  workerMgr.OnProduceWorkersDone := self.OnProducersDone;
  workerMgr.start(30, 60);
end;

procedure TfrmMain.btnBaseQueueConsumeClick(Sender: TObject);
begin
  FProduceCounter := 0;
  FConsumeCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.onConsume := onBaseQueueConsume;
  workerMgr.onProduce := onBaseQueueProduce;
  workerMgr.OnConsumeWorkersDone := self.OnConsumersDone;
  workerMgr.OnProduceWorkersDone := self.OnProducersDone;
  workerMgr.start(0, 1);
end;

procedure TfrmMain.btnDoublyLinkedClick(Sender: TObject);
var
  i:Integer;
begin
  FList.Clear;
  FProduceCounter := 0;
  FConsumeCounter := 0;
  FFailCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.onConsume := onDoublyLinkedConsume;
  workerMgr.onProduce := onDoublyLinkedProduce;
  workerMgr.OnConsumeWorkersDone := self.OnConsumersDone;
  workerMgr.OnProduceWorkersDone := self.OnProducersDone;
  workerMgr.start(50, 100);
end;

procedure TfrmMain.btnDoublyLinkedConsumeClick(Sender: TObject);
var
  i:Integer;
begin
  FProduceCounter := 0;
  FConsumeCounter := 0;
  FFailCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.onConsume := onDoublyLinkedConsume;
  workerMgr.onProduce := nil;
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
  FFailCounter := 0;
  workerMgr.checkIsDone();
  workerMgr.onConsume := onContextDoublyLinkedConsume;
  workerMgr.onProduce := onContextDoublyLinkedProduce;
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
  workerMgr.onConsume := onContextDoublyLinkedConsume;
  workerMgr.onProduce := nil;
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
  workerMgr.onConsume := OnLogWorker;
  workerMgr.onProduce := nil;
  workerMgr.OnConsumeWorkersDone := nil;
  workerMgr.OnProduceWorkersDone := nil;
  workerMgr.start(0, 20);

end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TfrmMain.onBaseQueueConsume(pvWoker: TThreadWorker);
var
  lvContext:TBaseDoublyObject;
  i, j:Integer;
begin
  i := 0;
  while not pvWoker.IsTerminated do
  begin
    lvContext := nil;
    lvContext := TBaseDoublyObject(FPool.Pop);

    if lvContext = nil then
    begin
      lvContext := nil;
      Break;
    end;

    InterlockedIncrement(FConsumeCounter);

    try
      lvContext.Free;
    except
      InterlockedIncrement(FFailCounter);
    end;
  end;
end;

procedure TfrmMain.onBaseQueueProduce(pvWoker: TThreadWorker);
var
  lvContext:TBaseDoublyObject;
  i, j:Integer;
begin
  for i := 1 to 3000 do
  begin
    lvContext := TBaseDoublyObject.Create;
//    FLocker2.Enter;
//    try
//      j := FCreateList.IndexOf(lvContext);
//      if j <> -1 then
//      begin
//        uiLogger.logMessage('create repeat object addr:%d', [j]);
//      end;
//      FCreateList.Add(lvContext);
//
//    finally
//      FLocker2.Leave;
//    end;
    FPool.Push(lvContext);
    InterlockedIncrement(FProduceCounter);
  end;
end;

procedure TfrmMain.onContextDoublyLinkedConsume(pvWoker:TThreadWorker);
var
  lvContext:TIocpClientContext;
begin
  while not pvWoker.IsTerminated do
  begin
    try
      lvContext := nil;
      lvContext := TIocpClientContext(FPool.Pop);
      if lvContext = nil then
      begin
        lvContext := nil;
        Break;
      end;
      FContextDoublyLinked.remove(lvContext);

      lvContext.Free;
      InterlockedIncrement(FConsumeCounter);
    except
      on E:Exception do
      begin
        uiLogger.logMessage(e.Message);
      end;
    end;
  end;
end;

procedure TfrmMain.OnConsumersDone(pvSender: tObject);
begin
  uiLogger.logMessage('=================================='
                      + sLineBreak + Format('consume counter:%d', [FConsumeCounter])
                      + sLineBreak + Format('produce counter:%d', [FProduceCounter])
                      + sLineBreak + Format('error counter:%d', [FFailCounter])
                      + sLineBreak + Format('ContextDoublyLinked count:%d', [FContextDoublyLinked.Count])
                      + sLineBreak + Format('SafeDoublyLinkedList count:%d', [FSafeDoublyLinkedList.Size])
                      + sLineBreak + Format('base queue:%d', [self.FPool.size])
                      );
end;

procedure TfrmMain.onDoublyLinkedConsume(pvWoker: TThreadWorker);
var
  lvContext:TBaseDoublyObject;
  i, j:Integer;
begin
  i := 0;
  while not pvWoker.IsTerminated do
  begin
    try
      lvContext := nil;

      lvContext := TBaseDoublyObject(FPool.Pop);

      if lvContext = nil then
      begin
        lvContext := nil;
        Break;
      end;

      InterlockedIncrement(FConsumeCounter);

      if not FSafeDoublyLinkedList.checkRemove(lvContext) then
      begin
        InterlockedIncrement(FFailCounter);
      end;

      lvContext.Free;
    except
      on E:Exception do
      begin
        uiLogger.logMessage(e.Message);
      end;
    end;
  end;

  //Assert(j = 3);
end;

procedure TfrmMain.onDoublyLinkedProduce(pvWoker: TThreadWorker);
var
  lvContext:TBaseDoublyObject;
  i:Integer;
begin
  for i := 1 to 3000 do
  begin
    lvContext := TBaseDoublyObject.Create;
    FSafeDoublyLinkedList.addToLast(lvContext);

    FPool.Push(lvContext);
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

procedure TfrmMain.onContextDoublyLinkedProduce(pvWoker:TThreadWorker);
var
  lvContext:TIocpClientContext;
  i:Integer;
begin
  for i := 1 to 3000 do
  begin
    lvContext := FTcpSvr.getClientContext;
    FContextDoublyLinked.add(lvContext);
    FPool.Push(lvContext);
    InterlockedIncrement(FProduceCounter);
  end;
end;

procedure TfrmMain.OnProducersDone(pvSender: tObject);
begin
end;

end.
