(*
 *	 Unit owner: D10.Mofen
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *	 v3.0.1(2014-7-16 21:36:30)
 *     + first release
 *
 *   thanks qsl's suggestion
 *)
unit iocpEngine;


interface

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

uses
  Windows, iocpProtocol, SysUtils, Classes, SyncObjs
  , ComObj, ActiveX, iocpLocker;


{$IF CompilerVersion> 23}
  {$define varNativeUInt}
{$IFEND}

{$if CompilerVersion >= 18}
  {$DEFINE INLINE}
{$IFEND}

const
  WORKER_ISBUSY =  $01;    // worker is busy
  WORKER_ISWATING = $02;   // waiting for task
  WORKER_RESERVED = $04;   // worker is reserved
  WORKER_OVER = $08;       // worker is dead


type
  TIocpRequest = class;
  TIocpEngine = class;
  TIocpWorker = class;

  POVERLAPPEDEx = ^OVERLAPPEDEx;
  OVERLAPPEDEx = packed record
    Overlapped: OVERLAPPED;
    iocpRequest: TIocpRequest;
    refCount: Integer;
  end;

  TThreadStackFunc = function(AThread:TThread):string;

  /// <summary>
  ///   iocp request root class
  /// </summary>
  TIocpRequest = class(TObject)
  private

    /// io request response info
    FiocpWorker: TIocpWorker;

    FPre: TIocpRequest;

    FRemark: String;

    // next Request
    FNext: TIocpRequest;
    
    FOnResponse: TNotifyEvent;
  protected
    FResponding: Boolean;
    FRespondStartTickCount:Cardinal;
    FRespondStartTime: TDateTime;
    FRespondEndTime: TDateTime;

    FErrorCode: Integer;

    //post request to iocp queue.
    FOverlapped: OVERLAPPEDEx;

    FBytesTransferred:NativeUInt;
    FCompletionKey:NativeUInt;


  protected

    procedure HandleResponse; virtual;

    function GetStateINfo: String; virtual;

    procedure ResponseDone; virtual;

    /// <summary>
    ///   cancel
    /// </summary>
    procedure CancelRequest; virtual;

  public
    constructor Create;

    property iocpWorker: TIocpWorker read FiocpWorker;

    property OnResponse: TNotifyEvent read FOnResponse write FOnResponse;

    property ErrorCode: Integer read FErrorCode;
    


    /// <summary>
    ///   remark
    /// </summary>
    property Remark: String read FRemark write FRemark;

    //
    property Responding: Boolean read FResponding;


  end;

  /// <summary>
  ///  request single link
  /// </summary>
  TIocpRequestSingleLink = class(TObject)
  private
    FCount: Integer;
    FHead: TIocpRequest;
    FTail: TIocpRequest;
    FMaxSize:Integer;
  public
    constructor Create(pvMaxSize: Integer = 1024);
    procedure SetMaxSize(pvMaxSize:Integer);
    destructor Destroy; override;
    function Push(pvRequest:TIocpRequest): Boolean;
    function Pop:TIocpRequest;
    property Count: Integer read FCount;
  end;

  /// <summary>
  ///  request doublyLinked
  /// </summary>
  TIocpRequestDoublyLinked = class(TObject)
  private
    FLocker: TIocpLocker;
    FHead: TIocpRequest;
    FTail: TIocpRequest;
    FCount:Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure add(pvContext: TIocpRequest);
    function remove(pvContext: TIocpRequest): Boolean;
    function Pop: TIocpRequest;
    procedure write2List(pvList:TList);  
    property Count: Integer read FCount; 
  end;

  /// <summary>
  ///   iocp core object
  ///     iocp core function
  /// </summary>
  TIocpCore = class(TObject)
  private
    /// <summary>
    ///   IOCP core handle
    /// </summary>
    FIOCPHandle: NativeUInt;

    // <summary>
    //   create IOCP handle
    // </summary>
    function createIOCPHandle: Boolean;

  public

    /// <summary>
    ///   binding a Handle to IOCPHandle
    /// </summary>
    function bind2IOCPHandle(pvHandle: THandle; pvCompletionKey: ULONG_PTR):
        THandle;

    /// <summary>
    ///   initialize engine
    ///     create iocp handle
    /// </summary>
    procedure doInitialize;

    /// <summary>
    ///   finalize engine
    /// </summary>
    procedure doFinalize;

    /// <summary>
    ///   handle io exception
    /// </summary>
    procedure HandleException(E:Exception);


    /// <summary>
    ///   post EXIT request into iocp queue
    /// </summary>
    function postIOExitRequest: Boolean;

    function postRequest(dwCompletionKey: DWORD; lpOverlapped: POverlapped):
        Boolean;
  end;

  /// <summary>
  ///    worker do process io request
  /// </summary>
  TIocpWorker = class(TThread)
  private
    FResponseCounter:Cardinal;

    FFlags: Integer;

    FIocpEngine: TIocpEngine;
    FIocpCore: TIocpCore;
    FCoInitialized:Boolean;

    FLastRequest:TIocpRequest;
  public
    constructor Create(AIocpCore: TIocpCore);
    
    procedure Execute; override;

    procedure writeStateINfo(const pvStrings: TStrings);

    procedure setFlag(pvFlag:Integer);{$IFDEF INLINE} inline; {$ENDIF}

    procedure removeFlag(pvFlag:Integer);{$IFDEF INLINE} inline; {$ENDIF}

    function checkFlag(pvFlag:Integer):Boolean;{$IFDEF INLINE} inline; {$ENDIF}

    /// <summary>
    ///   current worker invoke
    /// </summary>
    procedure checkCoInitializeEx(pvReserved: Pointer = nil; coInit: Longint = 0);

    /// <summary>
    ///   the last handle respond iocp request
    /// </summary>
    property LastRequest: TIocpRequest read FLastRequest;
  end;


  /// <summary>
  ///  iocp engine , mananger iocp workers.
  /// </summary>
  TIocpEngine = class(TObject)
  private
    FWorkerLocker: TIocpLocker;

    FMaxWorkerCount: Word;

    FActive: Boolean;

    // alive worker count
    FActiveWorkerCount:Integer;

    // iocp core object
    FIocpCore: TIocpCore;
    FName: String;

    // worker(thread) list
    FWorkerList: TList;

    // 
    FSafeStopSign: TEvent;

    // set worker count
    FWorkerCount: Word;

    /// <summary>
    ///   check worker thread is alive
    /// </summary>
    function workersIsAlive():Boolean;

    procedure incAliveWorker;
    procedure decAliveWorker(const pvWorker: TIocpWorker);
  public
    procedure writeStateINfo(const pvStrings:TStrings);

    function getStateINfo: String;

    /// <summary>
    ///   get worker handle response
    /// </summary>
    function getWorkerStateInfo(pvTimeOut: Cardinal = 3000): string;

    /// <summary>
    ///   get thread call stack
    /// </summary>
    function getWorkerStackInfos(pvThreadStackFunc: TThreadStackFunc; pvTimeOut:
        Cardinal = 3000): string;

    constructor Create;

    destructor Destroy; override;

    /// <summary>
    ///   set worker count, will clear and stop all workers
    /// </summary>
    procedure setWorkerCount(AWorkerCount: Integer);

    /// <summary>
    ///   set max worker count.
    /// </summary>
    procedure setMaxWorkerCount(AWorkerCount: Word);


    /// <summary>
    ///   check create a worker
    ///     true: create a worker
    /// </summary>
    function checkCreateWorker(pvIsTempWorker: Boolean): Boolean;

    /// <summary>
    ///   create worker thread and start worker
    /// </summary>
    procedure start;


    /// <summary>
    ///   stop and wait worker thread
    /// </summary>
    procedure safeStop;


    /// <summary>
    ///   check active, start
    /// </summary>
    procedure checkStart;



    property Active: Boolean read FActive;

    /// <summary>
    ///   core object, read only
    /// </summary>
    property IocpCore: TIocpCore read FIocpCore;


    property MaxWorkerCount: Word read FMaxWorkerCount write SetMaxWorkerCount;




    /// <summary>
    ///  get worker count
    /// </summary>
    property WorkerCount: Word read FWorkerCount;

    /// <summary>
    ///   Engine name
    /// </summary>
    property Name: String read FName write FName;


  end;

function IsDebugMode: Boolean;

implementation

{$IFDEF DEBUG_ON}
var
  workerCounter:Integer;
{$ENDIF}

resourcestring
  strDebugINfo               = 'active : %s, worker count: %d';
  strDebug_WorkerTitle       = '----------------------- woker %d --------------------';
  strDebug_Worker_INfo       = 'thread id: %d, response count: %d';
  strDebug_Worker_StateINfo  = 'busying:%s, waiting:%s, reserved:%s ';
  strDebug_Request_Title     = 'request state info:';


function IsDebugMode: Boolean;
begin
{$IFDEF MSWINDOWS}
{$warn symbol_platform off}
  Result := Boolean(DebugHook);
{$warn symbol_platform on}
{$ELSE}
  Result := false;
{$ENDIF}
end;

function getCPUCount: Integer;
{$IFDEF MSWINDOWS}
var
  si: SYSTEM_INFO;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  GetSystemInfo(si);
  Result := si.dwNumberOfProcessors;
  {$ELSE}// Linux,MacOS,iOS,Andriod{POSIX}
  {$IFDEF POSIX}
  Result := sysconf(_SC_NPROCESSORS_ONLN);
  {$ELSE}// unkown system, default 1
  Result := 1;
  {$ENDIF !POSIX}
  {$ENDIF !MSWINDOWS}
end;

function TIocpCore.bind2IOCPHandle(pvHandle: THandle; pvCompletionKey:
    ULONG_PTR): THandle;
begin
   Result := CreateIoCompletionPort(pvHandle, FIOCPHandle, pvCompletionKey, 0);
end;

function TIocpCore.createIOCPHandle: Boolean;
begin
  FIOCPHandle := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  Result := (FIOCPHandle <> 0) and (FIOCPHandle <> INVALID_HANDLE_VALUE);
  if not Result then
  begin
    RaiseLastOSError;
  end;
end;

procedure TIocpCore.doFinalize;
begin
  if FIOCPHandle <> 0 then CloseHandle(FIOCPHandle);
end;

procedure TIocpCore.doInitialize;
begin
  if FIOCPHandle = 0 then createIOCPHandle;
end;

procedure TIocpCore.HandleException(E:Exception);
begin
  ;
end;

function TIocpCore.postIOExitRequest: Boolean;
begin
  Result := PostQueuedCompletionStatus(FIOCPHandle, 0, 0, nil);
end;

function TIocpCore.postRequest(dwCompletionKey: DWORD; lpOverlapped:
    POverlapped): Boolean;
begin
  Result := PostQueuedCompletionStatus(FIOCPHandle, 0, dwCompletionKey, lpOverlapped);  
end;

procedure TIocpWorker.checkCoInitializeEx(pvReserved: Pointer = nil; coInit:
    Longint = 0);
begin
  if not FCoInitialized then
  begin
    CoInitializeEx(pvReserved, coInit);
    FCoInitialized := true;
  end;
end;

function TIocpWorker.checkFlag(pvFlag: Integer): Boolean;
begin
  Result := ((FFlags and pvFlag) <> 0);
end;

constructor TIocpWorker.Create(AIocpCore: TIocpCore);
begin
  inherited Create(True);
  FIocpCore := AIocpCore;
  FFlags := WORKER_RESERVED;  // default is reserved
  FResponseCounter := 0;
end;

{ TIocpWorker }

procedure TIocpWorker.Execute;
var
  lvBytesTransferred:ULONG_PTR;
  lvResultStatus:BOOL;
  lvErrCode:Integer;
  lpOverlapped:POVERLAPPEDEx;

  lpCompletionKey:ULONG_PTR;
  lvTempRequest:TIocpRequest;
begin
  FIocpEngine.incAliveWorker;

{$IFDEF DEBUG_ON}
  InterlockedIncrement(workerCounter);
{$ENDIF}

  while (not self.Terminated) do
  begin
    try
      FFlags := (FFlags or WORKER_ISWATING) and (not WORKER_ISBUSY);

      if checkFlag(WORKER_RESERVED) then
      begin
        lvResultStatus := GetQueuedCompletionStatus(FIocpCore.FIOCPHandle,
          lvBytesTransferred,  lpCompletionKey,
          POverlapped(lpOverlapped),
          INFINITE);
      end else
      begin
        // fire worker(will break) after 30's
        //   timeout will return false and break while
        lvResultStatus := GetQueuedCompletionStatus(FIocpCore.FIOCPHandle,
          lvBytesTransferred,  lpCompletionKey,
          POverlapped(lpOverlapped),
          30000);
      end;

      FFlags := (FFlags or WORKER_ISBUSY) and (not WORKER_ISWATING);


      if Assigned(lpOverlapped) then
      begin
        if not lvResultStatus then
        begin
          lvErrCode := GetLastError;
        end else
        begin
          lvErrCode := 0;
        end;

        Inc(FResponseCounter);

        lvTempRequest := lpOverlapped.iocpRequest;
        FLastRequest := lvTempRequest;
        try
          if FLastRequest = nil then
          begin
            Assert(FLastRequest<>NIL);
          end;
          /// reply io request, invoke handleRepsone to do ....

          lvTempRequest.FResponding := true;
          lvTempRequest.FRespondStartTime := Now();
          lvTempRequest.FRespondStartTickCount := GetTickCount;
          lvTempRequest.FRespondEndTime := 0;
          lvTempRequest.FiocpWorker := Self;
          lvTempRequest.FErrorCode := lvErrCode;
          lvTempRequest.FBytesTransferred := lvBytesTransferred;
          lvTempRequest.FCompletionKey := lpCompletionKey;
          if Assigned(lvTempRequest.FOnResponse) then
          begin
            lvTempRequest.FOnResponse(lvTempRequest);
          end else
          begin
            lvTempRequest.HandleResponse();
          end;
          lvTempRequest.FRespondEndTime := Now();
          lvTempRequest.FResponding := false;
        finally
          lvTempRequest.ResponseDone;
        end;

      end else
      begin
        /// exit
        Break;
      end;
    except
      on E: Exception do
      begin
        try
          FIocpCore.HandleException(E);
        except
        end;
      end;
    end;
    

  end;

  FFlags := WORKER_OVER;

  ///
  if FCoInitialized then CoUninitialize();

  
{$IFDEF DEBUG_ON}
  InterlockedDecrement(workerCounter);
{$ENDIF}

  //try
    FIocpEngine.decAliveWorker(Self);
//  except
//    Assert(False, ('iocpEngine name:' + FIocpEngine.Name));
//  end;
end;

procedure TIocpWorker.removeFlag(pvFlag: Integer);
begin
  FFlags := FFlags AND (not pvFlag);
end;

procedure TIocpWorker.setFlag(pvFlag: Integer);
begin
  FFlags := FFlags or pvFlag;
end;

procedure TIocpWorker.writeStateINfo(const pvStrings: TStrings);
var
  s:String;
begin
  pvStrings.Add(Format(strDebug_Worker_INfo, [self.ThreadID, FResponseCounter]));
  if checkFlag(WORKER_OVER) then
  begin
    pvStrings.Add('work done!!!');
  end else
  begin
    pvStrings.Add(Format(strDebug_Worker_StateINfo,
       [boolToStr(checkFlag(WORKER_ISBUSY), true),
        boolToStr(checkFlag(WORKER_ISWATING), true),
        boolToStr(checkFlag(WORKER_RESERVED), true)]));
        
    if (FLastRequest <> nil) then
    begin
      s := FLastRequest.getStateINfo;
      if s <> '' then
      begin
        pvStrings.Add(strDebug_Request_Title);
        pvStrings.Add(s);
      end;
    end;
  end;
end;

function TIocpEngine.checkCreateWorker(pvIsTempWorker: Boolean): Boolean;
var
  i:Integer;
  AWorker:TIocpWorker;
begin
  Result := false;
  FWorkerLocker.lock;
  try
    if FWorkerList.Count >= FMaxWorkerCount then exit;
    for i := 0 to FWorkerList.Count -1 do
    begin
      if TIocpWorker(FWorkerList[i]).checkFlag(WORKER_ISWATING) then
      begin
        Exit;
      end;
    end;

    AWorker := TIocpWorker.Create(FIocpCore);
    if pvIsTempWorker then
    begin
      AWorker.removeFlag(WORKER_RESERVED);
    end else
    begin
      AWorker.setFlag(WORKER_RESERVED);
    end;
    AWorker.FIocpEngine := Self;
    AWorker.FreeOnTerminate := True;
    FWorkerList.Add(AWorker);
    AWorker.Resume;

  finally
    FWorkerLocker.unLock;
  end;
end;

procedure TIocpEngine.checkStart;
begin
  if not FActive then start;
end;

constructor TIocpEngine.Create;
begin
  inherited Create;
  FWorkerLocker := TIocpLocker.Create;

  FWorkerCount := getCPUCount shl 2 - 1;
  FWorkerList := TList.Create();
  FIocpCore := TIocpCore.Create;
  FIocpCore.doInitialize;
end;

procedure TIocpEngine.decAliveWorker(const pvWorker: TIocpWorker);
begin
  FWorkerLocker.lock;
  try
    FWorkerList.Remove(pvWorker);
    InterlockedDecrement(FActiveWorkerCount);
    if FActiveWorkerCount = 0 then
    begin
      // all workers offline
      FSafeStopSign.SetEvent;
    end;
  finally
    FWorkerLocker.unLock;
  end;
end;

destructor TIocpEngine.Destroy;
begin
  safeStop;

  // wait thread's res back
  Sleep(10);

  FIocpCore.doFinalize;
  FIocpCore.Free;
  FreeAndNil(FWorkerList);
  FWorkerLocker.Free;
  inherited Destroy;
end;

function TIocpEngine.getStateINfo: String;
var
  lvStrings :TStrings;
begin
  lvStrings := TStringList.Create;
  try
    writeStateINfo(lvStrings);
    Result := lvStrings.Text;
  finally
    lvStrings.Free;
  end;
end;

function TIocpEngine.getWorkerStackInfos(pvThreadStackFunc: TThreadStackFunc;
    pvTimeOut: Cardinal = 3000): string;
var
  lvStrings :TStrings;
  i, j:Integer;
  lvWorker:TIocpWorker;
begin
  Assert(Assigned(pvThreadStackFunc));

  lvStrings := TStringList.Create;
  try
    j := 0;
    lvStrings.Add(Format(strDebugINfo, [BoolToStr(self.FActive, True), self.WorkerCount]));
    self.FWorkerLocker.lock;
    try
      for i := 0 to FWorkerList.Count - 1 do
      begin
        lvWorker := TIocpWorker(FWorkerList[i]);

        if lvWorker.checkFlag(WORKER_ISBUSY) then
        begin
          if GetTickCount - lvWorker.FLastRequest.FRespondStartTickCount > pvTimeOut then
          begin
            lvStrings.Add(Format(strDebug_WorkerTitle, [i + 1]));
            lvStrings.Add(pvThreadStackFunc(lvWorker));
            inc(j);
          end;
        end;
      end;
    finally
      self.FWorkerLocker.Leave;
    end;
    if j > 0 then
    begin
      Result := lvStrings.Text;
    end else
    begin
      Result := '';
    end;
  finally
    lvStrings.Free;
  end;
end;

function TIocpEngine.getWorkerStateInfo(pvTimeOut: Cardinal = 3000): string;
var
  lvStrings :TStrings;
  i, j:Integer;
  lvWorker:TIocpWorker;
begin
  lvStrings := TStringList.Create;
  try
    j := 0;
    lvStrings.Add(Format(strDebugINfo, [BoolToStr(self.FActive, True), self.WorkerCount]));
    self.FWorkerLocker.lock;
    try
      for i := 0 to FWorkerList.Count - 1 do
      begin
        lvWorker := TIocpWorker(FWorkerList[i]);

        if lvWorker.checkFlag(WORKER_ISBUSY) then
        begin
          if GetTickCount - lvWorker.FLastRequest.FRespondStartTickCount > pvTimeOut then
          begin
            lvStrings.Add(Format(strDebug_WorkerTitle, [i + 1]));
            lvWorker.writeStateINfo(lvStrings);
            inc(j);
          end;
        end;
      end;
    finally
      self.FWorkerLocker.Leave;
    end;
    if j > 0 then
    begin
      Result := lvStrings.Text;
    end else
    begin
      Result := '';
    end;
  finally
    lvStrings.Free;
  end;
end;

procedure TIocpEngine.incAliveWorker;
begin
  InterlockedIncrement(FActiveWorkerCount);
end;

procedure TIocpEngine.safeStop;
var
  i: Integer;
begin
  if FActive then
  begin
    if workersIsAlive then
    begin
      for i := 0 to FWorkerList.Count -1 do
      begin
        if not FIocpCore.postIOExitRequest then
        begin
          RaiseLastOSError;
        end;
      end;
    end else
    begin
      // all worker thread is dead

      FWorkerList.Clear;
      {$IFDEF DEBUG_ON}
      workerCounter := 0;
      {$ENDIF} 
      if FSafeStopSign <> nil then FSafeStopSign.SetEvent;
    end;

    if FSafeStopSign <> nil then
    begin
      // wait all works down
      FSafeStopSign.WaitFor(INFINITE);

      FSafeStopSign.Free;
      FSafeStopSign := nil;
    end;

    FWorkerList.Clear;
    FActive := false;
  end; 
end;

procedure TIocpEngine.setMaxWorkerCount(AWorkerCount: Word);
begin
  FMaxWorkerCount := AWorkerCount;

end;

procedure TIocpEngine.setWorkerCount(AWorkerCount: Integer);
begin
  if FActive then safeStop;
  if AWorkerCount <= 0 then
    FWorkerCount := (getCPUCount shl 2) -1
  else
    FWorkerCount := AWorkerCount;
  

end;

procedure TIocpEngine.start;
var
  i: Integer;
  AWorker: TIocpWorker;
  lvCpuCount:Integer;
begin
  lvCpuCount := getCPUCount;

  if FSafeStopSign <> nil then
  begin
    FSafeStopSign.Free;
  end;

  FSafeStopSign := TEvent.Create(nil, True, False, '');
  for i := 0 to FWorkerCount - 1 do
  begin
    AWorker := TIocpWorker.Create(FIocpCore);
    AWorker.FIocpEngine := Self;

    AWorker.FreeOnTerminate := True;
    AWorker.Resume;
    FWorkerList.Add(AWorker);

    // set worker use processor
    SetThreadIdealProcessor(AWorker.Handle, i mod lvCpuCount);
  end;
  FActive := true;
end;

function TIocpEngine.workersIsAlive: Boolean;
var
  i: Integer;
  lvCode:Cardinal;
begin
  Result := false;
  for i := FWorkerList.Count -1 downto 0 do
  begin
    if GetExitCodeThread(TThread(FWorkerList[i]).Handle, lvCode) then
    begin
      if lvCode=STILL_ACTIVE then
      begin
        Result := true;
        Break;
      end;
    end;
  end;

end;

procedure TIocpEngine.writeStateINfo(const pvStrings:TStrings);
var
  i:Integer;
begin
  pvStrings.Add(Format(strDebugINfo, [BoolToStr(self.FActive, True), self.WorkerCount]));

  self.FWorkerLocker.lock;
  try
    for i := 0 to FWorkerList.Count - 1 do
    begin
      pvStrings.Add(Format(strDebug_WorkerTitle, [i + 1]));
      TIocpWorker(FWorkerList[i]).writeStateINfo(pvStrings);
    end;
  finally
    self.FWorkerLocker.Leave;
  end;
end;

procedure TIocpRequest.CancelRequest;
begin
  
end;

constructor TIocpRequest.Create;
begin
  inherited Create;
  FOverlapped.iocpRequest := self;
  FOverlapped.refCount := 0;
end;

constructor TIocpRequestSingleLink.Create(pvMaxSize: Integer = 1024);
begin
  inherited Create;
  FMaxSize := pvMaxSize;
end;

destructor TIocpRequestSingleLink.Destroy;
begin
  inherited Destroy;
end;


function TIocpRequestSingleLink.Pop: TIocpRequest;
begin
  Result := nil;

  if FHead <> nil then
  begin
    Result := FHead;
    FHead := FHead.FNext;
    if FHead = nil then FTail := nil;

    Dec(FCount);
  end;

end;

function TIocpRequestSingleLink.Push(pvRequest:TIocpRequest): Boolean;
begin
  if FCount < FMaxSize then
  begin
    pvRequest.FNext := nil;

    if FHead = nil then
      FHead := pvRequest
    else
      FTail.FNext := pvRequest;

    FTail := pvRequest;

    Inc(FCount);
    Result := true;
  end else
  begin
    Result := false;
  end;

end;

procedure TIocpRequestSingleLink.SetMaxSize(pvMaxSize:Integer);
begin
  FMaxSize := pvMaxSize;
  if FMaxSize <=0 then FMaxSize := 10;
end;

procedure TIocpRequestDoublyLinked.add(pvContext: TIocpRequest);
begin
  FLocker.lock;
  try
    if FHead = nil then
    begin
      FHead := pvContext;
    end else
    begin
      FTail.FNext := pvContext;
      pvContext.FPre := FTail;
    end;

    FTail := pvContext;
    FTail.FNext := nil;

    inc(FCount);
  finally
    FLocker.unLock;
  end;
end;

constructor TIocpRequestDoublyLinked.Create;
begin
  inherited Create;
  FLocker := TIocpLocker.Create();
  FLocker.Name := 'onlineContext';
  FHead := nil;
  FTail := nil;
end;

destructor TIocpRequestDoublyLinked.Destroy;
begin
  FreeAndNil(FLocker);
  inherited Destroy;
end;

function TIocpRequestDoublyLinked.Pop: TIocpRequest;
begin
  FLocker.lock;
  try
    Result := FHead;
    if FHead <> nil then
    begin
      FHead := FHead.FNext;
      if FHead = nil then FTail := nil;
      Dec(FCount);
      Result.FPre := nil;
      Result.FNext := nil;  
    end;  
  finally
    FLocker.unLock;
  end;
end;

function TIocpRequestDoublyLinked.remove(pvContext: TIocpRequest): Boolean;
begin


  Result := false;
  FLocker.lock;
  try
//    if FCount = 0 then
//    begin
//      FCount := 0;
//    end;
    if pvContext.FPre <> nil then
    begin
      pvContext.FPre.FNext := pvContext.FNext;
      if pvContext.FNext <> nil then
        pvContext.FNext.FPre := pvContext.FPre;
    end else if pvContext.FNext <> nil then
    begin    // pre is nil, pvContext is FHead
      pvContext.FNext.FPre := nil;
      FHead := pvContext.FNext;
    end else
    begin   // pre and next is nil
      if pvContext = FHead then
      begin
        FHead := nil;
      end else
      begin
        exit;
      end;
    end;
    Dec(FCount);

    //  set pvConext.FPre is FTail
    if FTail = pvContext then FTail := pvContext.FPre;

    pvContext.FPre := nil;
    pvContext.FNext := nil;
    Result := true;
  finally
    FLocker.unLock;
  end;
end;

procedure TIocpRequestDoublyLinked.write2List(pvList: TList);
var
  lvItem:TIocpRequest;
begin
  FLocker.lock;
  try
    lvItem := FHead;
    while lvItem <> nil do
    begin
      pvList.Add(lvItem);
      lvItem := lvItem.FNext;
    end;
  finally
    FLocker.unLock;
  end;
end;

function TIocpRequest.GetStateINfo: String;
begin
  Result :=Format('%s %s', [Self.ClassName, FRemark]);
  if FResponding then
  begin
    Result :=Result + sLineBreak + Format('start:%s', [FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondStartTime)]);
  end else
  begin
    Result :=Result + sLineBreak + Format('start:%s, end:%s',
      [FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondStartTime),FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondEndTime)]);
  end;
end;

procedure TIocpRequest.HandleResponse;
begin
  
end;

procedure TIocpRequest.ResponseDone;
begin

end;

initialization
{$IFDEF DEBUG_ON}
  workerCounter := 0;
{$ENDIF}


finalization
{$IFDEF DEBUG_ON}
  if IsDebugMode then
    Assert(workerCounter = 0, ('iocpEngine workerCounter :' + IntToStr(workerCounter)));
{$ENDIF}

end.
