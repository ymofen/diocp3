(*
 *	 Unit owner: D10.Mofen
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *	 v3.0.1(2014-7-16 21:36:30)
 *     + first release
 *
 *
 *   2014-08-31 17:38:38
 *      感谢(流浪的虫子  419963966)对diocp3的捐赠
 *)
 
unit iocpBaseSocket;

interface

{$DEFINE LOGGER_ON}
{$DEFINE DEBUG_MSG_ON}

{$DEFINE CHANGE_STATE_USE_LOCKER}

uses
  Classes, iocpSocketUtils, iocpEngine, iocpProtocol,
  winsock, iocpWinsock2,

  iocpRawSocket, SyncObjs, Windows, SysUtils,
  {$IFDEF LOGGER_ON}
    safeLogger,
  {$ENDIF}

  BaseQueue, iocpLocker;


type
  TIocpBaseSocket = class;
  TIocpAcceptorMgr = class;
  TIocpBaseContext = class;
  TIocpRecvRequest = class;
  TIocpSendRequest = class;

  TIocpSendRequestClass = class of TIocpSendRequest;
  TIocpContextClass = class of TIocpBaseContext;

  TOnContextError = procedure(pvContext: TIocpBaseContext; pvErrorCode: Integer)
      of object;

  TOnBufferReceived = procedure(pvContext: TIocpBaseContext; buf: Pointer; len:
      cardinal; pvErrorCode: Integer) of object;
  
  TNotifyContextEvent = procedure(pvContext: TIocpBaseContext) of object;

  /// <summary>
  ///   on post request is completed
  /// </summary>
  TOnDataRequestCompleted = procedure(pvClientContext:TIocpBaseContext;
      pvRequest:TIocpRequest) of object;

  /// <summary>
  ///   client object
  /// </summary>
  TIocpBaseContext = class(TObject)
  private
    FActionLocker:TIocpLocker;

    FSendingLocker:TIocpLocker;
    FLastErrorCode:Integer;
    FDebugINfo: string;
    procedure SetDebugINfo(const Value: string);

  private
    FAlive:Boolean;

    // link
    FPre:TIocpBaseContext;
    FNext:TIocpBaseContext;

    /// <summary>
    ///   sending flag
    /// </summary>
    FSending: Boolean;

    FActive: Boolean;

    FOwner: TIocpBaseSocket;

    FRecvRequest:TIocpRecvRequest;

    FcurrSendRequest:TIocpSendRequest;

    FData: Pointer;
    FOnSocketStateChanged: TNotifyEvent;

    /// sendRequest link
    FSendRequestLink: TIocpRequestSingleLink;

    FRawSocket: TRawSocket;

    FRemoteAddr: String;

    FRemotePort: Integer;
    FSocketState: TSocketState;

    /// <summary>
    ///   called by recvRequest response
    /// </summary>
    procedure DoReceiveData;

    /// <summary>
    ///   called by sendRequest response
    /// </summary>
    procedure DoSendRequestCompleted(pvRequest: TIocpSendRequest);



    /// <summary>
    ///   post next sendRequest
    /// </summary>
    procedure checkNextSendRequest();

    /// <example>
    ///  sendRequest to pool
    /// </example>
    procedure checkReleaseRes;

    procedure SetOwner(const Value: TIocpBaseSocket);
  protected
    /// <summary>
    ///   request recv data
    /// </summary>
    procedure PostWSARecvRequest();

    /// <summary>
    ///   post reqeust to sending queue,
    ///    fail, push back to pool
    /// </summary>
    function postSendRequest(pvSendRequest:TIocpSendRequest):Boolean;


    /// <summary>
    ///
    /// </summary>
    function getSendRequest():TIocpSendRequest;


    procedure DoError(pvErrorCode:Integer);

  protected
    procedure DoCleanUp;virtual;

    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); virtual;

    procedure OnDisconnected; virtual;

    procedure OnConnected; virtual;

    procedure SetSocketState(pvState:TSocketState); virtual;

    /// <summary>
    ///   call in response event
    /// </summary>
    procedure DoConnected;

    /// <summary>
    ///   call in response event
    /// </summary>
    procedure DoDisconnect;

    procedure lock();

    procedure unLock();

  public
    procedure Close;

    constructor Create; virtual;

    destructor Destroy; override;
    /// <summary>
    ///  post send request to iocp queue, if post successful return true.
    ///    if request is completed, will call DoSendRequestCompleted procedure
    /// </summary>
    function PostWSASendRequest(buf:Pointer; len:Cardinal):Boolean;

    property Active: Boolean read FActive;

    property Data: Pointer read FData write FData;

    property DebugINfo: string read FDebugINfo write SetDebugINfo;

    property Owner: TIocpBaseSocket read FOwner write SetOwner;

    property RawSocket: TRawSocket read FRawSocket;

    property SocketState: TSocketState read FSocketState;

    property OnSocketStateChanged: TNotifyEvent read FOnSocketStateChanged write
        FOnSocketStateChanged;



  end;

  /// <summary>
  ///   WSARecv io request
  /// </summary>
  TIocpRecvRequest = class(TIocpRequest)
  private
    FInnerBuffer: iocpWinsock2.TWsaBuf;
    FRecvBuffer: iocpWinsock2.TWsaBuf;
    FRecvdFlag: Cardinal;
    FOwner: TIocpBaseSocket;
    FContext: TIocpBaseContext;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;

    /// <summary>
    ///   post recv request to iocp queue
    /// </summary>
    function PostRequest: Boolean; overload;

    /// <summary>
    ///
    /// </summary>
    function PostRequest(pvBuffer:PAnsiChar; len:Cardinal): Boolean; overload;

  public
    constructor Create;
    destructor Destroy; override;
  end;



  /// <summary>
  ///   WSASend io request
  /// </summary>
  TIocpSendRequest = class(TIocpRequest)
  private
    // for singlelinked
    FNext:TIocpSendRequest;

    FIsBusying:Boolean;

    FAlive: Boolean;

    FBytesSize:Cardinal;

    FCopyBuf:TWsaBuf;

    // send buf record
    FWSABuf:TWsaBuf;


    FBuf:Pointer;
    FPosition:Cardinal;
    FLen:Cardinal;

    FOwner: TIocpBaseSocket;

    FContext: TIocpBaseContext;


    FOnDataRequestCompleted: TOnDataRequestCompleted;
    /// <summary>
    ///   checkStart to post
    /// </summary>
    function checkStart: Boolean;
  protected
    /// <summary>
    ///   is all buf send completed?
    /// </summary>
    function isCompleted:Boolean;virtual;

    /// <summary>
    ///  on request successful
    /// </summary>
    procedure onSendRequestSucc; virtual;

    /// <summary>
    ///   post send a block
    /// </summary>
    function checkSendNextBlock: Boolean; virtual;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;


    procedure DoCleanUp;virtual;

    /// <summary>
    ///   post send buffer to iocp queue
    /// </summary>
    function InnerPostRequest(buf: Pointer; len: Cardinal): Boolean;

  public
    constructor Create; virtual;

    destructor Destroy; override;

    /// <summary>
    ///   set buf inneed to send
    /// </summary>
    procedure setBuffer(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean = true);

    property Owner: TIocpBaseSocket read FOwner;
    /// <summary>
    ///   on entire buf send completed
    /// </summary>
    property OnDataRequestCompleted: TOnDataRequestCompleted read
        FOnDataRequestCompleted write FOnDataRequestCompleted;
  end;


  /// <summary>
  ///   acceptEx request
  /// </summary>
  TIocpAcceptExRequest = class(TIocpRequest)
  private
    /// <summary>
    ///   acceptEx lpOutBuffer[in]
    ///     A pointer to a buffer that receives the first block of data sent on a new connection,
    ///       the local address of the server, and the remote address of the client.
    ///       The receive data is written to the first part of the buffer starting at offset zero,
    ///       while the addresses are written to the latter part of the buffer.
    ///       This parameter must be specified.
    /// </summary>
    FAcceptBuffer: array [0.. (SizeOf(TSockAddrIn) + 16) * 2 - 1] of byte;



    FOwner: TIocpBaseSocket;

    FAcceptorMgr:TIocpAcceptorMgr;

    FContext: TIocpBaseContext;
    FOnAcceptedEx: TNotifyEvent;
    /// <summary>
    ///   get socket peer info on acceptEx reponse
    /// </summary>
    procedure getPeerINfo;
  protected
    procedure HandleResponse; override;
    function PostRequest: Boolean;

  protected
  public
    constructor Create(AOwner: TIocpBaseSocket);
    property OnAcceptedEx: TNotifyEvent read FOnAcceptedEx write FOnAcceptedEx;
  end;

  /// <summary>
  ///   manager acceptEx request
  /// </summary>
  TIocpAcceptorMgr = class(TObject)
  private
    FListenSocket:TRawSocket;
    FOwner: TIocpBaseSocket;
    FList:TList;
    FLocker: TIocpLocker;
    FMaxRequest:Integer;
    FMinRequest:Integer;

  protected
  public
    constructor Create(AOwner: TIocpBaseSocket);
    destructor Destroy; override;

    procedure releaseRequestObject(pvRequest:TIocpAcceptExRequest);

    procedure removeRequestObject(pvRequest:TIocpAcceptExRequest);

    procedure checkPostRequest(pvContext: TIocpBaseContext);
  end;


  /// <summary>
  ///   connectEx io request
  /// </summary>
  TIocpConnectExRequest = class(TIocpRequest)
  private
    FBytesSent: DWORD;
  protected
    FContext: TIocpBaseContext;
  public
    /// <summary>
    ///   post connectEx request to iocp queue
    /// </summary>                                      l
    function PostRequest(pvHost: string; pvPort: Integer): Boolean;
  public
    constructor Create(AContext: TIocpBaseContext);
    destructor Destroy; override;
  end;


  /// <summary>
  ///   iocp data monitor
  /// </summary>
  TIocpDataMonitor = class(TObject)
  private
    FSentSize:Int64;
    FRecvSize:Int64;
    FPostWSASendSize: Int64;

    FPushSendQueueCounter: Integer;
    FResponseSendObjectCounter:Integer;

    FPostWSASendCounter:Integer;
    FResponseWSASendCounter:Integer;

    FPostWSARecvCounter:Integer;
    FResponseWSARecvCounter:Integer;

    FPostWSAAcceptExCounter:Integer;
    FResponseWSAAcceptExCounter:Integer;

    FLocker: TCriticalSection;
    FPostSendObjectCounter: Integer;

    procedure incSentSize(pvSize:Cardinal);
    procedure incPostWSASendSize(pvSize:Cardinal);
    procedure incRecvdSize(pvSize:Cardinal);

    procedure incPostWSASendCounter();
    procedure incResponseWSASendCounter;

    procedure incPostWSARecvCounter();
    procedure incResponseWSARecvCounter;

    procedure incPushSendQueueCounter;
    procedure incPostSendObjectCounter();
    procedure incResponseSendObjectCounter();
  public
    constructor Create;
    destructor Destroy; override;

    procedure clear;

    property PushSendQueueCounter: Integer read FPushSendQueueCounter;
    property PostSendObjectCounter: Integer read FPostSendObjectCounter;
    property ResponseSendObjectCounter: Integer read FResponseSendObjectCounter;

    property PostWSAAcceptExCounter: Integer read FPostWSAAcceptExCounter;
    property PostWSARecvCounter: Integer read FPostWSARecvCounter;
    property PostWSASendCounter: Integer read FPostWSASendCounter;


    property PostWSASendSize: Int64 read FPostWSASendSize;
    property RecvSize: Int64 read FRecvSize;

    property ResponseWSAAcceptExCounter: Integer read FResponseWSAAcceptExCounter;
    property ResponseWSARecvCounter: Integer read FResponseWSARecvCounter;
    property ResponseWSASendCounter: Integer read FResponseWSASendCounter;
    property SentSize: Int64 read FSentSize;
  end;

  TContextDoublyLinked = class(TObject)
  private
    FLocker: TIocpLocker;
    FHead:TIocpBaseContext;
    FTail:TIocpBaseContext;
    FCount:Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure add(pvContext:TIocpBaseContext);
    function remove(pvContext:TIocpBaseContext): Boolean;

    function Pop:TIocpBaseContext;

    procedure write2List(pvList:TList);

    property Count: Integer read FCount;

  end;


  TIocpBaseSocket = class(TComponent)
  private
  {$IFDEF LOGGER_ON}
    FSafeLogger:TSafeLogger;
  {$ENDIF}
    FIsDestroying :Boolean;
    FWSARecvBufferSize: Integer;
    procedure SetWSARecvBufferSize(const Value: Integer);

    function isDestroying:Boolean;
  {$IFDEF LOGGER_ON}
    function logCanWrite:Boolean;
  {$ENDIF}


  protected
    FContextClass:TIocpContextClass;

    FIocpSendRequestClass:TIocpSendRequestClass;
  private
    // sendRequest pool
    FSendRequestPool: TBaseQueue;

    /// data record
    FDataMoniter: TIocpDataMonitor;

    FActive: Boolean;

    FIocpEngine: TIocpEngine;

    FOnContextConnected: TNotifyContextEvent;
    FOnContextDisconnected: TNotifyContextEvent;

    FOnReceivedBuffer: TOnBufferReceived;


    FOnContextError: TOnContextError;

    // online clientcontext list
    FOnlineContextList: TContextDoublyLinked;

    FWSASendBufferSize: Integer;

    procedure DoClientContextError(pvClientContext: TIocpBaseContext;
        pvErrorCode: Integer);
    function GetWorkerCount: Integer;

    procedure SetWorkerCount(const Value: Integer);

    procedure SetActive(pvActive:Boolean);

    procedure DoReceiveData(pvIocpContext: TIocpBaseContext; pvRequest:
        TIocpRecvRequest);
  protected
    /// <summary>
    ///   pop sendRequest object
    /// </summary>
    function getSendRequest():TIocpSendRequest;

    /// <summary>
    ///   push back to pool
    /// </summary>
    function releaseSendRequest(pvObject:TIocpSendRequest): Boolean;

  protected
    /// <summary>
    ///   create Context Object
    /// </summary>
    function CreateContext: TIocpBaseContext;
    /// <summary>
    ///   occur on create instance
    /// </summary>
    procedure OnCreateContext(const context: TIocpBaseContext); virtual;
  private

    function GetOnlineContextCount: Integer;
    procedure SetWSASendBufferSize(const Value: Integer);

  public
    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    procedure registerContextClass(pvContextClass: TIocpContextClass);

    /// <summary>
    ///   create DataMonitor object
    /// </summary>
    procedure createDataMonitor;


    /// <summary>
    ///   check clientContext object is valid.
    /// </summary>
    function checkClientContextValid(const pvClientContext: TIocpBaseContext):  Boolean;

    /// <summary>
    ///   Stop and wait all workers down
    /// </summary>
    procedure Close;

    property Active: Boolean read FActive write SetActive;

    /// <summary>
    ///
    /// </summary>
    procedure DisconnectAll;
    procedure open();

    /// <summary>
    ///   get online client list
    /// </summary>
    procedure getOnlineContextList(pvList:TList);

    /// <summary>
    ///   client connections counter
    /// </summary>
    property OnlineContextCount: Integer read GetOnlineContextCount;

    property DataMoniter: TIocpDataMonitor read FDataMoniter;

    property IocpEngine: TIocpEngine read FIocpEngine;

  published

    /// <summary>
    ///   on disconnected
    /// </summary>
    property OnContextDisconnected: TNotifyContextEvent read FOnContextDisconnected
        write FOnContextDisconnected;

    /// <summary>
    ///   on connected
    /// </summary>
    property OnContextConnected: TNotifyContextEvent read FOnContextConnected write
        FOnContextConnected;

    /// <summary>
    ///   default cpu count * 2 -1
    /// </summary>
    property WorkerCount: Integer read GetWorkerCount write SetWorkerCount;


    /// <summary>
    ///   post wsaRecv request block size
    /// </summary>
    property WSARecvBufferSize: Integer read FWSARecvBufferSize write
        SetWSARecvBufferSize;


    /// <summary>
    ///   max size for post WSASend
    /// </summary>
    property WSASendBufferSize: Integer read FWSASendBufferSize write
        SetWSASendBufferSize;




    /// <summary>
    ///  on work error
    ///    occur in post request methods or iocp worker thread
    /// </summary>
    property OnContextError: TOnContextError read FOnContextError write
        FOnContextError;



    /// <summary>
    ///  on clientcontext received data
    ///    called by iocp worker thread
    /// </summary>
    property OnReceivedBuffer: TOnBufferReceived read FOnReceivedBuffer write
        FOnReceivedBuffer;
  end;

/// compare target, cmp_val same set target = new_val
/// return old value
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean):
    Boolean;

  

implementation

//{$IFDEF DEBUG_MSG_ON}
//procedure logDebugMessage(pvMsg: string; const args: array of const);
//begin
//  sfLogger.logMessage(pvMsg, args);
//end;
//{$ENDIF}



/// compare target, cmp_val same set target = new_val
/// return old value
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean):
    Boolean;
asm
{$ifdef win32}
  lock cmpxchg [ecx], dl
{$else}
.noframe
  mov rax, rcx
  lock cmpxchg [r8], dl
{$endif}
end;


procedure TIocpBaseContext.checkNextSendRequest;
var
  lvRequest:TIocpSendRequest;
begin
  FSendingLocker.lock();
  try
    lvRequest := TIocpSendRequest(FSendRequestLink.Pop);
    if lvRequest = nil then
    begin
      FSending := false;
    end;
  finally
    FSendingLocker.unLock;
  end;

  if lvRequest <> nil then
  begin
    FcurrSendRequest := lvRequest;
    if lvRequest.checkStart then
    begin
      if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.incPostSendObjectCounter;
      end;
    end else
    begin
      {$IFDEF DEBUG_MSG_ON}
       if FOwner.logCanWrite then
        FOwner.FSafeLogger.logMessage(
          'TIocpBaseContext.checkNextSendRequest.checkStart return false',  [],
            'DEBUG_', lgvDebug);
      {$ENDIF}

      /// close the Context
      FRawSocket.close;
    end;
  end;
end;

procedure TIocpBaseContext.checkReleaseRes;
var
  lvRequest:TIocpSendRequest;
begin
  while true do
  begin
    lvRequest :=TIocpSendRequest(FSendRequestLink.Pop);
    if lvRequest <> nil then
    begin
      FOwner.releaseSendRequest(lvRequest);
    end else
    begin
      Break;
    end;
  end;
end;

procedure TIocpBaseContext.Close;
begin
  FRawSocket.close;
end;

constructor TIocpBaseContext.Create;
begin
  inherited Create;
  FActionLocker := TIocpLocker.Create('context action locker');
  FSendingLocker := TIocpLocker.Create('sendinglocker');
  FAlive := False;
  FRawSocket := TRawSocket.Create();
  FActive := false;
  FSendRequestLink := TIocpRequestSingleLink.Create(10);
  FRecvRequest := TIocpRecvRequest.Create;
  FRecvRequest.FContext := self;
end;

destructor TIocpBaseContext.Destroy;
begin
  FRawSocket.close;
  FRawSocket.Free;

  FRecvRequest.Free;

  Assert(FSendRequestLink.Count = 0);
  FSendRequestLink.Free;
  FSendingLocker.Free;
  FActionLocker.Free;
  inherited Destroy;
end;

procedure TIocpBaseContext.DoCleanUp;
begin
  FOwner := nil;
  if FActive then
  begin
    FRawSocket.close;
    FActive := false;
    checkReleaseRes;
  end;
end;

procedure TIocpBaseContext.DoConnected;
begin
{$IFDEF CHANGE_STATE_USE_LOCKER}
  FActionLocker.lock('DoConnected');
  try
    if not FActive then
    begin
      Assert(FOwner <> nil);
      FActive := true;


      FOwner.FOnlineContextList.add(Self);
      try
        if Assigned(FOwner.FOnContextConnected) then
        begin
          FOwner.FOnContextConnected(Self);
        end;
        OnConnected();
      except
      end;

      SetSocketState(ssConnected);
      PostWSARecvRequest;
    end;
  finally
    FActionLocker.unLock;
  end;
{$ELSE}
  if lock_cmp_exchange(False, True, FActive) = False then
  begin
    Assert(FOwner <> nil);

    FOwner.FOnlineContextList.add(Self);
    try
      if Assigned(FOwner.FOnContextConnected) then
      begin
        FOwner.FOnContextConnected(Self);
      end;
      OnConnected();
    except
    end;

    SetSocketState(ssConnected);
    PostWSARecvRequest;
  end;
{$ENDIF}
end;

procedure TIocpBaseContext.DoDisconnect;
begin
{$IFDEF CHANGE_STATE_USE_LOCKER}
  FActionLocker.lock('discounnect');
  try
    if FActive then
    begin
      FActive := false;
      Assert(FOwner <> nil);
      FOwner.FOnlineContextList.remove(self);

      FRawSocket.close;
      checkReleaseRes;
      try
        if Assigned(FOwner.FOnContextDisconnected) then
        begin
          FOwner.FOnContextDisconnected(Self);
        end;
        OnDisconnected;
        SetSocketState(ssDisconnected);
      except
      end;
    end;
  finally
    FActionLocker.unLock;
  end;
{$ELSE}
  if lock_cmp_exchange(True, False, FActive) then
  begin
    Assert(FOwner <> nil);
    FOwner.FOnlineContextList.remove(self);

    FRawSocket.close;
    checkReleaseRes;
    try
      if Assigned(FOwner.FOnContextDisconnected) then
      begin
        FOwner.FOnContextDisconnected(Self);
      end;
      OnDisconnected;
      SetSocketState(ssDisconnected);
    except
    end;
  end;
{$ENDIF}
end;

procedure TIocpBaseContext.DoError(pvErrorCode: Integer);
begin
  FLastErrorCode:= pvErrorCode;
  FOwner.DoClientContextError(Self, pvErrorCode);
end;

procedure TIocpBaseContext.DoReceiveData;
begin
  OnRecvBuffer(FRecvRequest.FRecvBuffer.buf,
    FRecvRequest.FBytesTransferred,
    FRecvRequest.ErrorCode);
  if FOwner <> nil then
    FOwner.DoReceiveData(Self, FRecvRequest);
end;

procedure TIocpBaseContext.DoSendRequestCompleted(pvRequest:
    TIocpSendRequest);
begin
  ;
end;

function TIocpBaseContext.getSendRequest: TIocpSendRequest;
begin
  Result := FOwner.getSendRequest;
  Assert(Result <> nil);
  Result.FContext := self;
end;

procedure TIocpBaseContext.lock;
begin
  FActionLocker.lock();
end;

procedure TIocpBaseContext.OnConnected;
begin

end;

procedure TIocpBaseContext.OnDisconnected;
begin

end;





procedure TIocpBaseContext.OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode:
    WORD);
begin
  
end;

function TIocpBaseContext.postSendRequest(
  pvSendRequest: TIocpSendRequest): Boolean;
var
  lvDo:Boolean;
begin
  if FSendRequestLink.Push(pvSendRequest) then
  begin
    if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
    begin
      FOwner.FDataMoniter.incPushSendQueueCounter;
    end;

    Result := true;
    FSendingLocker.lock();
    try
      if not FSending then
      begin
        lvDo := true;
        FSending := true;
      end else
      begin
        lvDo := false;
      end;

    finally
      FSendingLocker.Leave;
    end;

    if lvDo then
    begin
      checkNextSendRequest;
    end;

//    if lock_cmp_exchange(False, True, FSending) = False then
//    begin
//      // start sending
//      checkNextSendRequest;
//    end;
  end else
  begin
  {$IFDEF DEBUG_MSG_ON}
    if FOwner.logCanWrite then
      FOwner.FSafeLogger.logMessage('Push sendRequest to Sending Queue fail, queue size:%d',
       [FSendRequestLink.Count], 'DEBUG_', lgvDebug);
  {$ENDIF}

    FOwner.releaseSendRequest(pvSendRequest);
    self.Close;
    Result := false;
  end;
end;

procedure TIocpBaseContext.PostWSARecvRequest;
begin
  FRecvRequest.PostRequest;
end;



function TIocpBaseContext.PostWSASendRequest(buf: Pointer; len: Cardinal): Boolean;
var
  lvRequest:TIocpSendRequest;
begin            
  lvRequest := getSendRequest;
  lvRequest.setBuffer(buf, len);
  Result := postSendRequest(lvRequest);
end;

procedure TIocpBaseContext.SetDebugINfo(const Value: string);
begin
  FDebugINfo := Value;
end;

procedure TIocpBaseContext.SetOwner(const Value: TIocpBaseSocket);
begin
  FOwner := Value;
  FRecvRequest.FOwner := FOwner;
end;

procedure TIocpBaseContext.SetSocketState(pvState:TSocketState);
begin
  FSocketState := pvState;
  if Assigned(FOnSocketStateChanged) then
  begin
    FOnSocketStateChanged(Self);
  end;
end;

procedure TIocpBaseContext.unLock;
begin
  FActionLocker.unLock;
end;

function TIocpBaseSocket.checkClientContextValid(const pvClientContext: TIocpBaseContext): Boolean;
begin
  Result := (pvClientContext.FOwner = Self);
end;

constructor TIocpBaseSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LOGGER_ON}
  FSafeLogger:=TSafeLogger.Create();
  FSafeLogger.setAppender(TLogFileAppender.Create(True));
{$ENDIF}
  FOnlineContextList := TContextDoublyLinked.Create();

  // send requestPool
  FSendRequestPool := TBaseQueue.Create;
    
  FIocpEngine := TIocpEngine.Create();


  // post wsaRecv block size
  FWSARecvBufferSize := 1024 * 4;

  FWSASendBufferSize := 1024 * 8;
end;

destructor TIocpBaseSocket.Destroy;
begin
{$IFDEF LOGGER_ON}
  FSafeLogger.Enable := false;
{$ENDIF}
  FIsDestroying := true;

  Close;

  if FDataMoniter <> nil then FDataMoniter.Free;

  FSendRequestPool.FreeDataObject;

  FOnlineContextList.Free;

  FIocpEngine.Free;

  FSendRequestPool.Free;
{$IFDEF LOGGER_ON}
  FSafeLogger.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TIocpBaseSocket.DoClientContextError(pvClientContext:
    TIocpBaseContext; pvErrorCode: Integer);
begin
  if Assigned(FOnContextError) then
    FOnContextError(pvClientContext, pvErrorCode);
end;

procedure TIocpBaseSocket.DoReceiveData(pvIocpContext: TIocpBaseContext;
    pvRequest: TIocpRecvRequest);
begin
  if Assigned(FOnReceivedBuffer) then
    FOnReceivedBuffer(pvIocpContext,
      pvRequest.FRecvBuffer.buf, pvRequest.FBytesTransferred,
      pvRequest.ErrorCode);
end;

function TIocpBaseSocket.GetWorkerCount: Integer;
begin
  Result := FIocpEngine.WorkerCount;
end;

function TIocpBaseSocket.isDestroying: Boolean;
begin
  Result := FIsDestroying or (csDestroying in self.ComponentState);
end;

{$IFDEF LOGGER_ON}
function TIocpBaseSocket.logCanWrite: Boolean;
begin
  Result := (not isDestroying) and FSafeLogger.Enable;
end;
{$ENDIF}


procedure TIocpBaseSocket.Open;
begin
  if FActive = true then exit;

  if FDataMoniter <> nil then FDataMoniter.clear;

  // engine start
  FIocpEngine.checkStart;

  FActive := True;
end;

procedure TIocpBaseSocket.registerContextClass(pvContextClass: TIocpContextClass);
begin
  FContextClass := pvContextClass;
end;

function TIocpBaseSocket.releaseSendRequest(pvObject:TIocpSendRequest): Boolean;
begin
  if lock_cmp_exchange(True, False, pvObject.FAlive) = True then
  begin
    FSendRequestPool.Push(pvObject);
    Result := true;
  end else
  begin
    Result := false;
  end;
end;

procedure TIocpBaseSocket.Close;
begin
  FActive := false;

  DisconnectAll;

  // engine Stop
  FIocpEngine.safeStop;
end;

procedure TIocpBaseSocket.SetActive(pvActive:Boolean);
begin
  if pvActive <> FActive then
  begin
    if pvActive then
    begin
      open;
    end else
    begin
      Close;
    end;
  end;
end;

procedure TIocpBaseSocket.SetWorkerCount(const Value: Integer);
begin
  FIocpEngine.setWorkerCount(Value);
end;

procedure TIocpBaseSocket.createDataMonitor;
begin
  if FDataMoniter = nil then
  begin
    FDataMoniter := TIocpDataMonitor.Create;
  end;
end;

function TIocpBaseSocket.CreateContext: TIocpBaseContext;
begin
  if FContextClass <> nil then
  begin
    Result := FContextClass.Create;
    OnCreateContext(Result);
  end else
  begin
    Result := TIocpBaseContext.Create;
    OnCreateContext(Result);
  end;

end;

procedure TIocpBaseSocket.DisconnectAll;
var
  lvContext:TIocpBaseContext;
begin
  while True do
  begin
    lvContext := FOnlineContextList.Pop;
    if lvContext <> nil then
    begin
      lvContext.DoDisconnect;
    end else
    begin
      Break;
    end;
  end;

end;

function TIocpBaseSocket.GetOnlineContextCount: Integer;
begin
  Result := FOnlineContextList.Count;
end;

procedure TIocpBaseSocket.getOnlineContextList(pvList:TList);
begin
  FOnlineContextList.write2List(pvList);
end;

function TIocpBaseSocket.getSendRequest: TIocpSendRequest;
begin
  Result := TIocpSendRequest(FSendRequestPool.Pop);
  if Result = nil then
  begin
    if FIocpSendRequestClass <> nil then
    begin
      Result := FIocpSendRequestClass.Create;
    end else
    begin
      Result := TIocpSendRequest.Create;
    end;
  end;
  Result.FAlive := true;
  Result.DoCleanup;
  Result.FOwner := Self;
end;

procedure TIocpBaseSocket.OnCreateContext(const context: TIocpBaseContext);
begin

end;

procedure TIocpBaseSocket.SetWSARecvBufferSize(const Value: Integer);
begin
  FWSARecvBufferSize := Value;
  if FWSARecvBufferSize = 0 then
  begin
    FWSARecvBufferSize := 1024 * 4;
  end;
end;

procedure TIocpBaseSocket.SetWSASendBufferSize(const Value: Integer);
begin
  FWSASendBufferSize := Value;
  if FWSASendBufferSize <=0 then
    FWSASendBufferSize := 1024 * 8;
end;

procedure TIocpAcceptorMgr.checkPostRequest(pvContext: TIocpBaseContext);
var
  lvRequest:TIocpAcceptExRequest;
begin
  FLocker.lock;
  try
    if FList.Count > FMinRequest then Exit;

    // post request
    while FList.Count < FMaxRequest do
    begin
      lvRequest := TIocpAcceptExRequest.Create(FOwner);
      lvRequest.FContext := pvContext;
      lvRequest.FAcceptorMgr := self;
      FList.Add(lvRequest);
      lvRequest.PostRequest;

      if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
      begin
        InterlockedIncrement(FOwner.FDataMoniter.FPostWSAAcceptExCounter);
      end;
    end;
  finally
    FLocker.unLock;
  end;
end;

constructor TIocpAcceptorMgr.Create(AOwner: TIocpBaseSocket);
begin
  inherited Create;
  FLocker := TIocpLocker.Create();
  FLocker.Name := 'acceptorLocker';
  FMaxRequest := 200;
  FMinRequest := 10;  
  FList := TList.Create;
  FOwner := AOwner;
end;

destructor TIocpAcceptorMgr.Destroy;
begin
  FList.Free;
  FLocker.Free;
  inherited Destroy;
end;

procedure TIocpAcceptorMgr.releaseRequestObject(
  pvRequest: TIocpAcceptExRequest);
begin
  pvRequest.Free; 
end;

procedure TIocpAcceptorMgr.removeRequestObject(pvRequest: TIocpAcceptExRequest);
begin
  FLocker.lock;
  try
    FList.Remove(pvRequest);
  finally
    FLocker.unLock;
  end;
end;

constructor TIocpAcceptExRequest.Create(AOwner: TIocpBaseSocket);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TIocpAcceptExRequest.getPeerINfo;
var
  localAddr: PSockAddr;
  remoteAddr: PSockAddr;
  localAddrSize : Integer;
  remoteAddrSize : Integer;
begin
  localAddrSize := SizeOf(TSockAddr) + 16;
  remoteAddrSize := SizeOf(TSockAddr) + 16;
  IocpGetAcceptExSockaddrs(@FAcceptBuffer[0],
                        0,
                        SizeOf(localAddr^) + 16,
                        SizeOf(remoteAddr^) + 16,
                        localAddr,
                        localAddrSize,
                        remoteAddr,
                        remoteAddrSize);

  FContext.FRemoteAddr := string(inet_ntoa(TSockAddrIn(remoteAddr^).sin_addr));
  FContext.FRemotePort := ntohs(TSockAddrIn(remoteAddr^).sin_port);
end;

procedure TIocpAcceptExRequest.HandleResponse;
begin
  if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
  begin
    InterlockedIncrement(FOwner.FDataMoniter.FResponseWSAAcceptExCounter);
  end;

  try
    if ErrorCode = 0 then
    begin
      // msdn
      // The socket sAcceptSocket does not inherit the properties of the socket
      //  associated with sListenSocket parameter until SO_UPDATE_ACCEPT_CONTEXT
      //  is set on the socket.
      FAcceptorMgr.FListenSocket.UpdateAcceptContext(FContext.FRawSocket.SocketHandle);

      getPeerINfo();
    end;
    if Assigned(FOnAcceptedEx) then FOnAcceptedEx(Self);
  finally
    FAcceptorMgr.releaseRequestObject(Self);
  end;
end;

function TIocpAcceptExRequest.PostRequest: Boolean;
var
  dwBytes: Cardinal;
  lvRet:BOOL;
  lvErrCode:Integer;
  lp:POverlapped;
begin
  FContext.FRawSocket.createTcpOverlappedSocket;
  dwBytes := 0;
  lp := @FOverlapped;
  lvRet := IocpAcceptEx(FAcceptorMgr.FListenSocket.SocketHandle
                , FContext.FRawSocket.SocketHandle
                , @FAcceptBuffer[0]
                , 0
                , SizeOf(TSockAddrIn) + 16
                , SizeOf(TSockAddrIn) + 16
                , dwBytes
                , lp);
  if not lvRet then
  begin
    lvErrCode := WSAGetLastError;
    Result := lvErrCode = WSA_IO_PENDING;
    if not Result then
    begin
      FOwner.DoClientContextError(FContext, lvErrCode);
    end;
  end else
  begin
    Result := True;
  end;
end;

constructor TIocpRecvRequest.Create;
begin
  inherited Create;
end;

destructor TIocpRecvRequest.Destroy;
begin
  if FInnerBuffer.len > 0 then
  begin
    FreeMem(FInnerBuffer.buf, FInnerBuffer.len);
  end;
  inherited Destroy;
end;

procedure TIocpRecvRequest.HandleResponse;
begin
  if (FOwner.FDataMoniter <> nil) then
  begin
    FOwner.FDataMoniter.incResponseWSARecvCounter;
    FOwner.FDataMoniter.incRecvdSize(FBytesTransferred);
  end;

  if ErrorCode <> 0 then
  begin
  {$IFDEF DEBUG_MSG_ON}
    if FOwner.logCanWrite then
      FOwner.FSafeLogger.logMessage('IocpRecvRequest response ErrorCode:%d',
        [ErrorCode], 'DEBUG_', lgvDebug);
  {$ENDIF}
    if FOwner <> nil then
    begin
      FContext.DoError(ErrorCode);

      FContext.DoDisconnect;
    end else
    begin
      Self.Free;
    end;
  end else if (FBytesTransferred = 0) then
  begin      // no data recvd, socket is break
  {$IFDEF DEBUG_MSG_ON}
    if FOwner.logCanWrite then
      FOwner.FSafeLogger.logMessage('IocpRecvRequest response FBytesTransferred is zero',
       [], 'DEBUG_', lgvDebug);
  {$ENDIF}
    FContext.DoDisconnect;
  end else
  begin
    try
      FContext.DoReceiveData;
    finally
      // post recv request
      if FOwner.Active then
      begin
        if FContext.FActive then
        begin
          // post recv request again
          FContext.PostWSARecvRequest;
        end;
      end else
      begin
      {$IFDEF DEBUG_MSG_ON}
        if FOwner.logCanWrite then
          FOwner.FSafeLogger.logMessage('IocpRecvRequest response Owner is deactive'
           , 'DEBUG_', lgvDebug);
      {$ENDIF}
        FContext.DoDisconnect;
      end;
    end;
  end;
end;

function TIocpRecvRequest.PostRequest(pvBuffer: PAnsiChar;
  len: Cardinal): Boolean;
var
  lvRet:Integer;
  lpNumberOfBytesRecvd: Cardinal;
begin
  lpNumberOfBytesRecvd := 0;
  FRecvdFlag := 0;

  FRecvBuffer.buf := pvBuffer;
  FRecvBuffer.len := len;

  lvRet := iocpWinsock2.WSARecv(FContext.FRawSocket.SocketHandle,
     @FRecvBuffer,
     1,
     lpNumberOfBytesRecvd,
     FRecvdFlag,
     LPWSAOVERLAPPED(@FOverlapped),   // d7 need to cast
     nil
     );
  if lvRet = SOCKET_ERROR then
  begin
    lvRet := WSAGetLastError;
    Result := lvRet = WSA_IO_PENDING;
    if not Result then
    begin
      {$IFDEF DEBUG_MSG_ON}
       if FOwner.logCanWrite then
        FOwner.FSafeLogger.logMessage(
          'TIocpRecvRequest.PostRequest Error:%d',  [lvRet],
            'DEBUG_', lgvDebug);
      {$ENDIF}

      // trigger error event
      FOwner.DoClientContextError(FContext, lvRet);

      // kick out clientContext
      FContext.DoDisconnect;
    end else
    begin
      if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.incPostWSARecvCounter;
      end;
    end;
  end else
  begin
    Result := True;
    
    if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
    begin
      FOwner.FDataMoniter.incPostWSARecvCounter;
    end;
  end;   
end;

function TIocpRecvRequest.PostRequest: Boolean;
begin
  if FInnerBuffer.len <> FOwner.FWSARecvBufferSize then
  begin
    if FInnerBuffer.len > 0 then FreeMem(FInnerBuffer.buf);
    FInnerBuffer.len := FOwner.FWSARecvBufferSize;
    GetMem(FInnerBuffer.buf, FInnerBuffer.len);
  end;
  Result := PostRequest(FInnerBuffer.buf, FInnerBuffer.len);
end;

function TIocpSendRequest.checkSendNextBlock: Boolean;
var
  l:Cardinal;
begin
  if FPosition < FLen then
  begin
    l := FLen - FPosition;
    if l > FOwner.FWSASendBufferSize then l := FOwner.FWSASendBufferSize;

    Result := InnerPostRequest(Pointer(IntPtr(FBuf) + IntPtr(FPosition)), l)

  end else
  begin
    Result := false;
  end;
end;

constructor TIocpSendRequest.Create;
begin
  inherited Create;
end;

destructor TIocpSendRequest.Destroy;
begin
  if FCopyBuf.len <> 0 then
  begin
    FreeMem(FCopyBuf.buf);
  end;
  inherited Destroy;
end;

procedure TIocpSendRequest.DoCleanUp;
begin
  FBytesSize := 0;
  FNext := nil;
  FOwner := nil;
  FContext := nil;
  FBuf := nil;
  FLen := 0;
  FPosition := 0;
end;

procedure TIocpSendRequest.HandleResponse;
begin
  FIsBusying := false;
  if FOwner = nil then
  begin
    exit;
  end else if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
  begin                                                       
//    if (ErrorCode = 0) and (FBytesTransferred <> FWSABuf.len) then  /// error
//    begin
//       FBytesTransferred := FBytesTransferred;
//    end;
    FOwner.FDataMoniter.incSentSize(FBytesTransferred);
    FOwner.FDataMoniter.incResponseWSASendCounter;

  end;

  if ErrorCode <> 0 then
  begin
   {$IFDEF DEBUG_MSG_ON}
    if FOwner.logCanWrite then
      FOwner.FSafeLogger.logMessage(
        'TIocpSendRequest.HandleResponse ErrorCode:%d',  [ErrorCode],
          'DEBUG_', lgvDebug);
   {$ENDIF}
    FOwner.DoClientContextError(FContext, ErrorCode);
    FContext.DoDisconnect;

    // release request
    FOwner.releaseSendRequest(Self);
  end else
  begin
    onSendRequestSucc;
    if isCompleted then    // is all buf send completed?
    begin
      if FOwner.FDataMoniter <> nil then
      begin
        FOwner.FDataMoniter.incResponseSendObjectCounter;
      end;

      FContext.checkNextSendRequest;

      if Assigned(FOnDataRequestCompleted) then
      begin
        FOnDataRequestCompleted(FContext, Self);
      end;

      FContext.DoSendRequestCompleted(Self);

      // release request
      FOwner.releaseSendRequest(Self);
    end else
    begin
      if not checkSendNextBlock then
      begin
        {$IFDEF DEBUG_MSG_ON}
         if FOwner.logCanWrite then
          FOwner.FSafeLogger.logMessage(
            'TIocpSendRequest.checkSendNextBlock return false',  [],
              'DEBUG_', lgvDebug);
        {$ENDIF}

        /// kick out the clientContext
        FContext.DoDisconnect;
      end;
    end;
  end;
end;

function TIocpSendRequest.InnerPostRequest(buf: Pointer; len: Cardinal):
    Boolean;
var
  lvRet: Integer;
  dwFlag: Cardinal;
  lpNumberOfBytesSent:Cardinal;
begin
  FIsBusying := True;
  FBytesSize := len;
  FWSABuf.buf := buf;
  FWSABuf.len := len;
  dwFlag := 0;
  lpNumberOfBytesSent := 0;
  lvRet := WSASend(FContext.FRawSocket.SocketHandle,
                    @FWSABuf,
                    1,
                    lpNumberOfBytesSent,
                    dwFlag,
                    LPWSAOVERLAPPED(@FOverlapped),   // d7 need to cast
                    nil
  );
  if lvRet = SOCKET_ERROR then
  begin
    lvRet := WSAGetLastError;
    Result := lvRet = WSA_IO_PENDING;
    if not Result then
    begin
       FIsBusying := False;
    {$IFDEF DEBUG_MSG_ON}
       if FOwner.logCanWrite then
          FOwner.FSafeLogger.logMessage(
            'TIocpSendRequest.InnerPostRequest Error:%d', [lvRet],
              'DEBUG_', lgvDebug);
    {$ENDIF}

       FOwner.DoClientContextError(FContext, lvRet);

       /// kick out the clientContext
       FContext.DoDisconnect;

       FOwner.releaseSendRequest(Self);
    end else
    begin
      if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.incPostWSASendSize(len);
        FOwner.FDataMoniter.incPostWSASendCounter;
      end;
    end;
  end else
  begin
    Result := True;
    if (FOwner <> nil) and (FOwner.FDataMoniter <> nil) then
    begin
      FOwner.FDataMoniter.incPostWSASendSize(len);
      FOwner.FDataMoniter.incPostWSASendCounter;
    end;
  end;
end;

function TIocpSendRequest.isCompleted: Boolean;
begin
  Result := FPosition >= FLen;
end;

procedure TIocpSendRequest.onSendRequestSucc;
begin
  FPosition := FPosition + self.FBytesTransferred;
end;

procedure TIocpSendRequest.setBuffer(buf: Pointer; len: Cardinal; pvCopyBuf:
    Boolean = true);
begin
  if pvCopyBuf then
  begin
    if FCopyBuf.len > 0 then FreeMem(FCopyBuf.buf);

    FCopyBuf.len := len;
    GetMem(FCopyBuf.buf, FCopyBuf.len);
    Move(buf^, FCopyBuf.buf^, FCopyBuf.len);
    FBuf := FCopyBuf.buf;
    FLen := FCopyBuf.len;
  end else
  begin
    FBuf := buf;
    FLen := len;
  end;
  FPosition := 0;
end;

function TIocpSendRequest.checkStart: Boolean;
begin
  Result := checkSendNextBlock;
end;

procedure TIocpDataMonitor.clear;
begin
  FLocker.Enter;
  try
    FSentSize:=0;
    FRecvSize:=0;
    FPostWSASendSize:=0;

    FPostWSASendCounter:=0;
    FResponseWSASendCounter:=0;

    FPostWSARecvCounter:=0;
    FResponseWSARecvCounter:=0;

    FPushSendQueueCounter := 0;
    FResponseSendObjectCounter := 0;
    
    FPostWSAAcceptExCounter:=0;
    FResponseWSAAcceptExCounter:=0;
  finally
    FLocker.Leave;
  end;
end;

constructor TIocpDataMonitor.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create();
end;

destructor TIocpDataMonitor.Destroy;
begin
  FLocker.Free;
  inherited Destroy;
end;

procedure TIocpDataMonitor.incPushSendQueueCounter;
begin
  InterlockedIncrement(FPushSendQueueCounter);
end;

procedure TIocpDataMonitor.incPostSendObjectCounter;
begin
  InterlockedIncrement(FPostSendObjectCounter);
end;

procedure TIocpDataMonitor.incPostWSARecvCounter;
begin
  InterlockedIncrement(FPostWSARecvCounter);
end;

procedure TIocpDataMonitor.incPostWSASendCounter;
begin
  InterlockedIncrement(FPostWSASendCounter);
end;

procedure TIocpDataMonitor.incPostWSASendSize(pvSize: Cardinal);
begin
  FLocker.Enter;
  try
    FPostWSASendSize := FPostWSASendSize + pvSize;
  finally
    FLocker.Leave;
  end;
end;

procedure TIocpDataMonitor.incRecvdSize(pvSize: Cardinal);
begin
  FLocker.Enter;
  try
    FRecvSize := FRecvSize + pvSize;
  finally
    FLocker.Leave;
  end;
end;

procedure TIocpDataMonitor.incResponseSendObjectCounter;
begin
  InterlockedIncrement(FResponseSendObjectCounter);
end;

procedure TIocpDataMonitor.incResponseWSARecvCounter;
begin
  InterlockedIncrement(FResponseWSARecvCounter);
end;

procedure TIocpDataMonitor.incResponseWSASendCounter;
begin
  InterlockedIncrement(FResponseWSASendCounter);
end;

procedure TIocpDataMonitor.incSentSize(pvSize:Cardinal);
begin
  FLocker.Enter;
  try
    FSentSize := FSentSize + pvSize;
  finally
    FLocker.Leave;
  end;
end;

procedure TContextDoublyLinked.add(pvContext: TIocpBaseContext);
begin
  FLocker.lock;
  try
    if FHead = nil then
    begin
      FHead := pvContext;
    end else
    begin
      if FTail = nil then
      begin
        FCount := FCount;
      end;
      FTail.FNext := pvContext;
      pvContext.FPre := FTail;
    end;

    FTail := pvContext;
    FTail.FNext := nil;

    if FTail = nil then
    begin
      FCount := FCount;
    end;

    inc(FCount);
  finally
    FLocker.unLock;
  end;
end;

constructor TContextDoublyLinked.Create;
begin
  inherited Create;
  FCount := 0;
  FLocker := TIocpLocker.Create();
  FLocker.Name := 'onlineContext';
  FHead := nil;
  FTail := nil;
end;

destructor TContextDoublyLinked.Destroy;
begin
  FreeAndNil(FLocker);
  inherited Destroy;
end;

function TContextDoublyLinked.Pop: TIocpBaseContext;
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

function TContextDoublyLinked.remove(pvContext:TIocpBaseContext): Boolean;
begin

  Result := false;
  FLocker.lock;
  try
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

    if FCount < 0 then
    begin
      Assert(FCount > 0);
    end;

    //  set pvConext.FPre is FTail
    if FTail = pvContext then
      FTail := pvContext.FPre;

    if FTail = nil then
    begin
      FCount := FCount;
      FTail := nil;
    end;

    if FHead = nil then
    begin
      FCount := FCount;
      FHead := nil;
    end;

    pvContext.FPre := nil;
    pvContext.FNext := nil;
    Result := true;
  finally
    FLocker.unLock;
  end;
end;

procedure TContextDoublyLinked.write2List(pvList: TList);
var
  lvItem:TIocpBaseContext;
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

constructor TIocpConnectExRequest.Create(AContext: TIocpBaseContext);
begin
  inherited Create;
  FContext := AContext;
end;

destructor TIocpConnectExRequest.Destroy;
begin
  inherited Destroy;
end;

function TIocpConnectExRequest.PostRequest(pvHost: string; pvPort: Integer):
    Boolean;
var
  lvSockAddrIn:TSockAddrIn;
  lvRet:BOOL;
  lvErrCode:Integer;
  lp:Pointer;
begin
  FContext.setSocketState(ssConnecting);
  lvSockAddrIn := iocpSocketUtils.getSocketAddr(pvHost, pvPort);

  FContext.RawSocket.bind('0.0.0.0', 0);

  lp :=@FOverlapped;
  lvRet := iocpSocketUtils.IocpConnectEx(FContext.RawSocket.SocketHandle,
        @lvSockAddrIn
        , sizeOf(lvSockAddrIn)
        , nil
        , 0
        , FBytesSent
        , lp
        );
  if not lvRet then
  begin
    lvErrCode := WSAGetLastError;
    Result := lvErrCode = WSA_IO_PENDING;
    if not Result then
    begin
      FContext.DoError(lvErrCode);
      FContext.DoDisconnect;
    end;
  end else
  begin
    Result := True;
  end;

end;

end.
