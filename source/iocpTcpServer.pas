(*
 *	 Unit owner: D10.Mofen
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *	 v3.0.1(2014-7-16 21:36:30)
 *     + first release
 *
 *   thanks qsl's suggestion
 *)
 
unit iocpTcpServer;

interface

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

{$DEFINE SOCKET_REUSE}

{$DEFINE USE_HASHTABLE}

uses
  Classes, iocpSocketUtils, iocpEngine, iocpProtocol,
  winsock, iocpWinsock2,

  iocpRawSocket, SyncObjs, Windows, SysUtils,
  safeLogger,
  {$IFDEF USE_HASHTABLE}
    DHashTable,
  {$ENDIF}
  BaseQueue, iocpLocker;

const
  SOCKET_HASH_SIZE = $FFFF;

  CORE_LOG_FILE = 'diocp_core_exception';


type
  TIocpTcpServer = class;
  TIocpAcceptorMgr = class;
  TIocpClientContext = class;
  TIocpRecvRequest = class;
  TIocpSendRequest = class;
  TIocpDisconnectExRequest = class;

  TIocpClientContextClass = class of TIocpClientContext;

  TOnClientContextError = procedure(pvClientContext: TIocpClientContext; errCode:
      Integer) of object;

  TOnDataReceived = procedure(pvClientContext:TIocpClientContext;
      buf:Pointer; len:cardinal; errCode:Integer) of object;

  TOnContextAcceptEvent = procedure(pvSocket: THandle; pvAddr: String; pvPort:
      Integer; var vAllowAccept: Boolean) of object;

  TClientContextNotifyEvent = procedure(pvClientContext: TIocpClientContext) of
      object;

  /// <summary>
  ///   on post request is completed
  /// </summary>
  TOnDataRequestCompleted = procedure(pvClientContext:TIocpClientContext;
      pvRequest:TIocpRequest) of object;

  /// <summary>
  ///   client object
  /// </summary>
  TIocpClientContext = class(TObject)
  private
    FSocketHandle:TSocket;
    
    FDebugStrings:TStrings;
    {$IFDEF SOCKET_REUSE}
    FDisconnectExRequest:TIocpDisconnectExRequest;
    {$ENDIF}

    FSocketState: TSocketState;

    /// <summary>
    ///   ReferenceCounter counter
    /// </summary>
    FReferenceCounter:Integer;

    /// <summary>
    ///    request discnnect flag, ReferenceCounter is zero then do disconnect
    /// </summary>
    FRequestDisconnect:Boolean;

    FContextLocker: TIocpLocker;

    FDebugINfo: string;
    procedure SetDebugINfo(const Value: string);



    function incReferenceCounter(pvDebugInfo: string; pvObj: TObject): Boolean;

    /// <summary>
    ///    dec RequestCounter then check counter and Request flag for Disonnect
    /// </summary>
    function decReferenceCounter(pvDebugInfo: string; pvObj: TObject): Integer;

    /// <summary>
    ///   dec RequestCounter and requestDisconnect then check counter flag for Disonnect
    /// </summary>
    procedure decReferenceCounterAndRequestDisconnect(pvDebugInfo: string; pvObj:
        TObject);

    procedure lock();
    procedure unLock();

  {$IFDEF SOCKET_REUSE}
    /// <summary>
    ///
    /// </summary>
    procedure OnDisconnectExResponse(pvObject:TObject);
  {$ENDIF}
  private
    FAlive:Boolean;


    // socket/context map
    FPreForHash:TIocpClientContext;
    FNextForHash:TIocpClientContext;


    // link
    FPre:TIocpClientContext;
    FNext:TIocpClientContext;

    /// <summary>
    ///  sending flag
    /// </summary>
    FSending: Boolean;

    FActive: Boolean;

    FOwner: TIocpTcpServer;



    FcurrSendRequest:TIocpSendRequest;
    FData: Pointer;

    /// sendRequest link
    FSendRequestLink: TIocpRequestSingleLink;

    FRawSocket: TRawSocket;

    FRemoteAddr: String;
    FRemotePort: Integer;

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


    procedure SetOwner(const Value: TIocpTcpServer);



  protected
    /// <summary>
    ///   recvRequest
    /// </summary>
    FRecvRequest:TIocpRecvRequest;

    /// <summary>
    ///   request recv data
    /// </summary>
    procedure PostWSARecvRequest();virtual;

    /// <summary>
    ///   post reqeust to sending queue,
    ///    fail, push back to pool
    /// </summary>
    function postSendRequest(pvSendRequest:TIocpSendRequest):Boolean;


    /// <summary>
    ///
    /// </summary>
    function getSendRequest():TIocpSendRequest;

    procedure InnerCloseContext;
  protected
    /// <summary>
    ///   lock context avoid disconnect,
    ///     lock succ return false else return false( context request disconnect)
    /// </summary>
    function LockContext(pvDebugInfo: string; pvObj: TObject): Boolean;

    procedure unLockContext(pvDebugInfo: string; pvObj: TObject);

    procedure DoConnected;

    procedure DoCleanUp;virtual;

    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); virtual;

    procedure OnDisconnected; virtual;

    procedure OnConnected;virtual;

    procedure SetSocketState(pvState:TSocketState); virtual;
  public
    procedure postNextSendRequest;


    constructor Create; virtual;
    destructor Destroy; override;

    procedure DoDisconnect;

    procedure RequestDisconnect;

    /// <summary>
    ///  post send request to iocp queue, if post successful return true.
    ///    if request is completed, will call DoSendRequestCompleted procedure
    /// </summary>
    function PostWSASendRequest(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean =
        true): Boolean;

    property Active: Boolean read FActive;

    property Data: Pointer read FData write FData;

    property DebugINfo: string read FDebugINfo write SetDebugINfo;

    property Owner: TIocpTcpServer read FOwner write SetOwner;

    property RawSocket: TRawSocket read FRawSocket;

    property RemoteAddr: String read FRemoteAddr;

    property RemotePort: Integer read FRemotePort;
    property SocketHandle: TSocket read FSocketHandle;
    property SocketState: TSocketState read FSocketState;

  end;



  /// <summary>
  ///   WSARecv io request
  /// </summary>
  TIocpRecvRequest = class(TIocpRequest)
  private
    FDebugInfo:String;
    FInnerBuffer: iocpWinsock2.TWsaBuf;
    FRecvBuffer: iocpWinsock2.TWsaBuf;
    FRecvdFlag: Cardinal;
    FOwner: TIocpTcpServer;
    FClientContext:TIocpClientContext;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;
  public
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


  TIocpSendRequestClass = class of TIocpSendRequest;
  /// <summary>
  ///   WSASend io request
  /// </summary>
  TIocpSendRequest = class(TIocpRequest)
  private
    /// <summary>
    ///   can return to pool
    /// </summary>
    FCanGiveBack:Boolean;
    
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

    FOwner: TIocpTcpServer;

    FClientContext:TIocpClientContext; 

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


    procedure ResponseDone; override;


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

    property ClientContext: TIocpClientContext read FClientContext;

    property Owner: TIocpTcpServer read FOwner;

    /// <summary>
    ///   on entire buf send completed
    /// </summary>
    property OnDataRequestCompleted: TOnDataRequestCompleted read
        FOnDataRequestCompleted write FOnDataRequestCompleted;
  end;

  TIocpDisconnectExRequest = class(TIocpRequest)
  private
    FOwner:TIocpTcpServer;
    FContext:TIocpClientContext;

  protected
    function PostRequest: Boolean;

    /// <summary>
    ///   directly post request,
    /// </summary>
    function DirectlyPost: Boolean;

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

    FOwner: TIocpTcpServer;
    FAcceptorMgr:TIocpAcceptorMgr;

    FClientContext:TIocpClientContext;
    /// <summary>
    ///   get socket peer info on acceptEx reponse
    /// </summary>
    procedure getPeerINfo;
  protected
    function PostRequest: Boolean;

  protected
    procedure HandleResponse; override;

    procedure ResponseDone; override;

  public
    constructor Create(AOwner: TIocpTcpServer);
  end;

  /// <summary>
  ///   manager acceptEx request
  /// </summary>
  TIocpAcceptorMgr = class(TObject)
  private
    FOwner: TIocpTcpServer;
    FList:TList;
    FListenSocket: TRawSocket;
    FLocker: TIocpLocker;
    FMaxRequest:Integer;
    FMinRequest:Integer;

  protected
  public
    constructor Create(AOwner: TIocpTcpServer; AListenSocket: TRawSocket);

    destructor Destroy; override;

    procedure releaseRequestObject(pvRequest:TIocpAcceptExRequest);

    procedure removeRequestObject(pvRequest:TIocpAcceptExRequest);

    procedure checkPostRequest;

    property ListenSocket: TRawSocket read FListenSocket;


  end;

  /// <summary>
  ///   iocp data monitor
  /// </summary>
  TIocpDataMonitor = class(TObject)
  private
    FSentSize:Int64;
    FRecvSize:Int64;
    FPostWSASendSize: Int64;

    FHandleCreateCounter:Integer;
    FHandleDestroyCounter:Integer;

    FContextCreateCounter: Integer;
    FContextOutCounter:Integer;
    FContextReturnCounter:Integer;

    FPushSendQueueCounter: Integer;
    FResponseSendObjectCounter:Integer;

    FSendRequestCreateCounter: Integer;
    FSendRequestOutCounter:Integer;
    FSendRequestReturnCounter:Integer;

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
    {$IFDEF SOCKET_REUSE}
    procedure incHandleCreateCounter;
    procedure incHandleDestroyCounter;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    procedure clear;

    property ContextCreateCounter: Integer read FContextCreateCounter;
    property ContextOutCounter: Integer read FContextOutCounter;
    property ContextReturnCounter: Integer read FContextReturnCounter;
    property HandleCreateCounter: Integer read FHandleCreateCounter;
    property HandleDestroyCounter: Integer read FHandleDestroyCounter;
    property Locker: TCriticalSection read FLocker;
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
    property SendRequestCreateCounter: Integer read FSendRequestCreateCounter;
    property SendRequestOutCounter: Integer read FSendRequestOutCounter;
    property SendRequestReturnCounter: Integer read FSendRequestReturnCounter;
    property SentSize: Int64 read FSentSize;
  end;

  TContextDoublyLinked = class(TObject)
  private
    FLocker: TIocpLocker;
    FHead:TIocpClientContext;
    FTail:TIocpClientContext;
    FCount:Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure add(pvContext:TIocpClientContext);
    function indexOf(pvContext: TIocpClientContext): Integer;
    function remove(pvContext:TIocpClientContext): Boolean;

    function Pop:TIocpClientContext;

    procedure write2List(pvList:TList);

    property Count: Integer read FCount;

  end;

  {$IF RTLVersion>22}
  // thanks: Âó×ÓÖÙ·Ê19183455
  //  vcl for win64
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TIocpTcpServer = class(TComponent)
  private
    FSafeLogger:TSafeLogger;

    FLocker:TIocpLocker;

    FMaxSendingQueueSize:Integer;

    FIsDestroying :Boolean;
    FWSARecvBufferSize: cardinal;
    procedure SetWSARecvBufferSize(const Value: cardinal);

    function isDestroying:Boolean;
    function logCanWrite:Boolean;
  protected
    FClientContextClass:TIocpClientContextClass;

    FIocpSendRequestClass:TIocpSendRequestClass;

    procedure SetName(const NewName: TComponentName); override;
  private
    // clientContext pool
    FContextPool: TBaseQueue;


    // sendRequest pool
    FSendRequestPool: TBaseQueue;

    // extend data
    FDataPtr: Pointer;

    /// data record
    FDataMoniter: TIocpDataMonitor;

    FActive: Boolean;

  {$IFDEF USE_HASHTABLE}
    FOnlineContextList : TDHashTable;
  {$ELSE}
    // onlinie hash list
    FClientsHash: array [0..SOCKET_HASH_SIZE] of TIocpClientContext;
    // online clientcontext list
    FOnlineContextList: TContextDoublyLinked;
  {$ENDIF}


    // acceptEx request mananger
    FIocpAcceptorMgr:TIocpAcceptorMgr;

    FIocpEngine: TIocpEngine;

    FKeepAlive: Boolean;

    // server listen socket, accept client connection
    FListenSocket: TRawSocket;
    FOnClientContextConnected: TClientContextNotifyEvent;
    FOnClientContextDisconnected: TClientContextNotifyEvent;


    FOnDataReceived: TOnDataReceived;


    FOnClientContextError: TOnClientContextError;
    FOnContextAccept: TOnContextAcceptEvent;

    FPort: Integer;

    FWSASendBufferSize: cardinal;

    procedure DoClientContextError(pvClientContext: TIocpClientContext;
        pvErrorCode: Integer);
    function GetWorkerCount: Integer;

    procedure SetWorkerCount(const Value: Integer);

    procedure SetActive(pvActive:Boolean);



    /// <summary>
    ///   occur on create instance
    /// </summary>
    procedure onCreateClientContext(const context: TIocpClientContext);virtual;

    /// <summary>
    ///   pop ClientContext object
    /// </summary>
    function getClientContext():TIocpClientContext;

    /// <summary>
    ///   giveback to pool
    /// </summary>
    function releaseClientContext(pvObject:TIocpClientContext): Boolean;



    procedure AddToOnlineList(pvObject:TIocpClientContext);

    procedure RemoveFromOnOnlineList(pvObject:TIocpClientContext);



    procedure doReceiveData(pvIocpClientContext:TIocpClientContext; pvRequest:TIocpRecvRequest);
  protected
    /// <summary>
    ///   pop sendRequest object
    /// </summary>
    function getSendRequest():TIocpSendRequest;

    /// <summary>
    ///   push back to pool
    /// </summary>
    function releaseSendRequest(pvObject:TIocpSendRequest): Boolean;

  private
    procedure DoAcceptExResponse(pvRequest: TIocpAcceptExRequest);

    function GetClientCount: Integer;
    procedure SetWSASendBufferSize(const Value: cardinal);

  public
    constructor Create(AOwner: TComponent); override;

    procedure setMaxSendingQueueSize(pvSize:Integer);

    destructor Destroy; override;

    /// <summary>
    ///   according socket handle
    /// </summary>
    function findContext(pvSocketHandle:TSocket):TIocpClientContext;

    procedure registerContextClass(pvContextClass: TIocpClientContextClass);

    procedure registerSendRequestClass(pvClass: TIocpSendRequestClass);

    /// <summary>
    ///   create DataMonitor object
    /// </summary>
    procedure createDataMonitor;


    /// <summary>
    ///   check clientContext object is valid.
    /// </summary>
    function checkClientContextValid(const pvClientContext: TIocpClientContext):  Boolean;

    /// <summary>
    ///
    /// </summary>
    procedure DisconnectAll;

    /// <summary>
    ///   wait for all conntext is off
    /// </summary>
    function WaitForContext(pvTimeOut: Cardinal): Boolean;

    /// <summary>
    ///   get online client list
    /// </summary>
    procedure getOnlineContextList(pvList:TList);

    /// <summary>
    ///   stop and wait all workers down
    /// </summary>
    procedure safeStop();

    property Active: Boolean read FActive write SetActive;

    procedure open();

    procedure close;



    /// <summary>
    ///   client connections counter
    /// </summary>
    property ClientCount: Integer read GetClientCount;
    property DataMoniter: TIocpDataMonitor read FDataMoniter;
    property IocpEngine: TIocpEngine read FIocpEngine;

    /// <summary>
    ///   set socket Keep alive option when acceptex
    /// </summary>
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;

    /// <summary>
    ///   extend data
    /// </summary>
    property DataPtr: Pointer read FDataPtr write FDataPtr;
  published

    /// <summary>
    ///   on disconnected
    /// </summary>
    property OnClientContextDisconnected: TClientContextNotifyEvent read
        FOnClientContextDisconnected write FOnClientContextDisconnected;

    /// <summary>
    ///   on connected
    /// </summary>
    property OnClientContextConnected: TClientContextNotifyEvent read FOnClientContextConnected write FOnClientContextConnected;

    /// <summary>
    ///   
    /// </summary>
    property OnContextAccept: TOnContextAcceptEvent read FOnContextAccept write
        FOnContextAccept;
    /// <summary>
    ///   listen port
    /// </summary>
    property Port: Integer read FPort write FPort;

    /// <summary>
    ///   default cpu count * 2 -1
    /// </summary>
    property WorkerCount: Integer read GetWorkerCount write SetWorkerCount;


    /// <summary>
    ///   post wsaRecv request block size
    /// </summary>
    property WSARecvBufferSize: cardinal read FWSARecvBufferSize write
        SetWSARecvBufferSize;

    /// <summary>
    ///   max size for post WSASend
    /// </summary>
    property WSASendBufferSize: cardinal read FWSASendBufferSize write
        SetWSASendBufferSize;









    /// <summary>
    ///  on work error
    ///    occur in post request methods or iocp worker thread
    /// </summary>
    property OnClientContextError: TOnClientContextError read FOnClientContextError
        write FOnClientContextError;



    /// <summary>
    ///  on clientcontext received data
    ///    called by iocp worker thread
    /// </summary>
    property OnDataReceived: TOnDataReceived read FOnDataReceived write
        FOnDataReceived;
  end;



implementation




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


procedure TIocpClientContext.InnerCloseContext;
begin
  Assert(FOwner <> nil);
{$IFDEF DEBUG_ON}
  if FReferenceCounter <> 0 then
    if FOwner.logCanWrite then
    FOwner.FSafeLogger.logMessage('InnerCloseContext FReferenceCounter:%d', [FReferenceCounter], 'exception_Debug_');
  if not FActive then
  begin
    if FOwner.logCanWrite then
      FOwner.FSafeLogger.logMessage('InnerCloseContext FActive is false', 'exception_Debug_');
    exit;
  end;
{$ENDIF}
//  Assert(FReferenceCounter = 0);
//  Assert(FActive);
  try
    FActive := false;
  {$IFDEF SOCKET_REUSE}
    if (FOwner.FDataMoniter <> nil) then
      FOwner.FDataMoniter.incHandleDestroyCounter;
  {$ELSE}
    FRawSocket.close;
  {$ENDIF}

    checkReleaseRes;

    FOwner.RemoveFromOnOnlineList(Self);
    try
      if Assigned(FOwner.FOnClientContextDisconnected) then
      begin
        FOwner.FOnClientContextDisconnected(Self);
      end;
      OnDisconnected;
    except
    end;
  finally
    FOwner.releaseClientContext(Self);
  end;

end;

procedure TIocpClientContext.lock;
begin
  FContextLocker.lock();
end;

function TIocpClientContext.LockContext(pvDebugInfo: string; pvObj: TObject):
    Boolean;
begin
  Result := incReferenceCounter(pvDebugInfo, pvObj);
end;

procedure TIocpClientContext.unLockContext(pvDebugInfo: string; pvObj: TObject);
begin
  if Self = nil then
  begin
    Assert(Self<> nil);
  end;
  decReferenceCounter(pvDebugInfo, pvObj);
end;


procedure TIocpClientContext.checkNextSendRequest;
var
  lvRequest:TIocpSendRequest;
begin
  Assert(FOwner <> nil);
  lvRequest := TIocpSendRequest(FSendRequestLink.Pop);
  if lvRequest = nil then
  begin
    FSending := false;
  end;

  if lvRequest <> nil then
  begin
    FcurrSendRequest := lvRequest;
    if lvRequest.checkStart then
    begin
      if (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.incPostSendObjectCounter;
      end;
    end else
    begin
    {$IFDEF DEBUG_ON}
      if FOwner.logCanWrite then
        FOwner.FSafeLogger.logMessage('TIocpClientContext.checkNextSendRequest.checkStart return false',  []);
    {$ENDIF}

      /// kick out the clientContext
      RequestDisconnect;
    end;
  end;
end;

procedure TIocpClientContext.checkReleaseRes;
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

constructor TIocpClientContext.Create;
begin
  inherited Create;
  FDebugStrings := TStringList.Create;
  FReferenceCounter := 0;
  FContextLocker := TIocpLocker.Create('contextLocker');
  FAlive := False;
  FRawSocket := TRawSocket.Create();
  FActive := false;
  FSendRequestLink := TIocpRequestSingleLink.Create(10);
  FRecvRequest := TIocpRecvRequest.Create;
  FRecvRequest.FClientContext := self;

  {$IFDEF SOCKET_REUSE}
  FDisconnectExRequest:=TIocpDisconnectExRequest.Create;
  FDisconnectExRequest.FContext := Self;
  FDisconnectExRequest.OnResponse := OnDisconnectExResponse;
  {$ENDIF}
end;

function TIocpClientContext.incReferenceCounter(pvDebugInfo: string; pvObj:
    TObject): Boolean;
begin
  FContextLocker.lock('incReferenceCounter');
  if (not Active) or FRequestDisconnect then
  begin
    Result := false;
  end else
  begin
    Inc(FReferenceCounter);
    FDebugStrings.Add(Format('+(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));

    if FDebugStrings.Count > 20 then FDebugStrings.Delete(0);

    Result := true;
  end;
  FContextLocker.unLock;
end;


function TIocpClientContext.decReferenceCounter(pvDebugInfo: string; pvObj:
    TObject): Integer;
var
  lvCloseContext:Boolean;
begin
  lvCloseContext := false;
  FContextLocker.lock('decReferenceCounter');
  Dec(FReferenceCounter);
  Result := FReferenceCounter;
  FDebugStrings.Add(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));

  if FDebugStrings.Count > 20 then
    FDebugStrings.Delete(0);


  if FReferenceCounter < 0 then
    Assert(FReferenceCounter >=0 );
  if FReferenceCounter = 0 then
    if FRequestDisconnect then lvCloseContext := true;
    
  FContextLocker.unLock; 
  
  if lvCloseContext then InnerCloseContext; 
end;

procedure TIocpClientContext.decReferenceCounterAndRequestDisconnect(
    pvDebugInfo: string; pvObj: TObject);
var
  lvCloseContext:Boolean;
begin
  lvCloseContext := false;

  FContextLocker.lock('decReferenceCounter');
  FRequestDisconnect := true;
  Dec(FReferenceCounter);
  FDebugStrings.Add(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));

  if FDebugStrings.Count > 20 then
    FDebugStrings.Delete(0);
  if FReferenceCounter < 0 then
    Assert(FReferenceCounter >=0 );
  if FReferenceCounter = 0 then
    lvCloseContext := true;
    
  FContextLocker.unLock; 
  
  if lvCloseContext then InnerCloseContext;
end;

procedure TIocpClientContext.RequestDisconnect;
var
  lvCloseContext:Boolean;
begin
  FContextLocker.lock('RequestDisconnect');
  
  {$IFDEF SOCKET_REUSE}
  lvCloseContext := False;
  if not FRequestDisconnect then
  begin
    // cancel
    FRawSocket.ShutDown();

    // post succ, in handleReponse Event do
    if not FDisconnectExRequest.PostRequest then
    begin      // post fail,
      FRawSocket.close;
      if FReferenceCounter = 0 then  lvCloseContext := true;    //      lvCloseContext := true;   //directly close
    end;
    FRequestDisconnect := True;
  end;
  {$ELSE}
  lvCloseContext := False;
  FRequestDisconnect := True;
  if FReferenceCounter = 0 then  lvCloseContext := true;
  {$ENDIF}

  FContextLocker.unLock;

  {$IFDEF SOCKET_REUSE}
  if lvCloseContext then InnerCloseContext;
  {$ELSE}
  if lvCloseContext then InnerCloseContext else FRawSocket.close;
  {$ENDIF}

end;

destructor TIocpClientContext.Destroy;
begin
  if FReferenceCounter <> 0 then
  begin
    Assert(FReferenceCounter = 0);
  end;

  if FSendRequestLink.Count > 0 then
  begin
    Assert(FSendRequestLink.Count = 0);
  end;

  FRawSocket.close;
  FRawSocket.Free;

  FRecvRequest.Free;

  Assert(FSendRequestLink.Count = 0);

  {$IFDEF SOCKET_REUSE}
  FDisconnectExRequest.Free;
  {$ENDIF}

  FSendRequestLink.Free;
  FContextLocker.Free;
  FDebugStrings.Free;
  inherited Destroy;
end;

procedure TIocpClientContext.DoCleanUp;
begin
  FOwner := nil;
  Assert(FReferenceCounter = 0);
  FRequestDisconnect := false;
  FSending := false;

  FDebugStrings.Add(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(Self), '-----DoCleanUp-----']));

  Assert(not FActive);
//  if FActive then
//  begin
//    FRawSocket.close;
//    FActive := false;
//    checkReleaseRes;
//  end;
end;

procedure TIocpClientContext.DoConnected;
begin
  FContextLocker.lock('DoConnected');
  try
    FSocketHandle := FRawSocket.SocketHandle;
    Assert(FOwner <> nil);
    if FActive then
    begin
      {$IFDEF DEBUG_ON}
        if FOwner.logCanWrite then
          FOwner.FSafeLogger.logMessage('on DoConnected event is already actived', 'exception_Debug_');
      {$ENDIF}
    end else
    begin
      FActive := true;
      FOwner.AddToOnlineList(Self);

      if self.LockContext('onConnected', Self) then
      try
        if Assigned(FOwner.FOnClientContextConnected) then
        begin
          FOwner.FOnClientContextConnected(Self);
        end;

        try
          OnConnected();
        except
        end;

        PostWSARecvRequest;
      finally
        self.unLockContext('OnConnected', Self);
      end;
    end;
  finally
    FContextLocker.unLock;
  end;
end;

procedure TIocpClientContext.DoDisconnect;
begin
  RequestDisconnect;
end;

procedure TIocpClientContext.DoReceiveData;
begin
  OnRecvBuffer(FRecvRequest.FRecvBuffer.buf,
    FRecvRequest.FBytesTransferred,
    FRecvRequest.FErrorCode);
  if FOwner <> nil then
    FOwner.doReceiveData(Self, FRecvRequest);
end;

procedure TIocpClientContext.DoSendRequestCompleted(pvRequest:
    TIocpSendRequest);
begin
  ;
end;

function TIocpClientContext.getSendRequest: TIocpSendRequest;
begin
  Result := FOwner.getSendRequest;
  Assert(Result <> nil);
  Result.FClientContext := self;
end;






procedure TIocpClientContext.OnConnected;
begin
  
end;

procedure TIocpClientContext.OnDisconnected;
begin

end;


{$IFDEF SOCKET_REUSE}
procedure TIocpClientContext.OnDisconnectExResponse(pvObject:TObject);
var
  lvRequest:TIocpDisconnectExRequest;
begin
  if FActive then
  begin   // already connected
    lvRequest :=TIocpDisconnectExRequest(pvObject);
    if lvRequest.FErrorCode <> 0 then
    begin
      RawSocket.close;
      if (FOwner.FDataMoniter <> nil) then
        FOwner.FDataMoniter.incHandleDestroyCounter;
      decReferenceCounter(
          Format('TIocpDisconnectExRequest.HandleResponse.Error, %d', [lvRequest.FErrorCode])
          , lvRequest
        );
    end else
    begin
      decReferenceCounter(
          'TIocpDisconnectExRequest.HandleResponse', lvRequest
        );
    end;
  end else
  begin
    // not connected, onaccept allow is false
    FOwner.releaseClientContext(Self)
  end;
end;
{$ENDIF}


procedure TIocpClientContext.OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode:
    WORD);
begin
  
end;

procedure TIocpClientContext.postNextSendRequest;
begin
  self.lock;
  try
    checkNextSendRequest;
  finally
    self.unLock;
  end;
end;

function TIocpClientContext.postSendRequest(
  pvSendRequest: TIocpSendRequest): Boolean;
begin
  Result := False;

  if incReferenceCounter('TIocpClientContext.postSendRequest', pvSendRequest) then
  begin
    try
      FContextLocker.lock();
      try
        Result := FSendRequestLink.Push(pvSendRequest);
        if Result then
        begin
          if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
          begin
            FOwner.FDataMoniter.incPushSendQueueCounter;
          end;
          Result := true;

          if not FSending then
          begin
            FSending := true;
            checkNextSendRequest;
          end;
        end;
      finally
        FContextLocker.unLock;
      end;

      if not Result then
      begin
      {$IFDEF DEBUG_ON}
        if FOwner.logCanWrite then
          FOwner.FSafeLogger.logMessage('Push sendRequest to Sending Queue fail, queue size:%d',
           [FSendRequestLink.Count]);
      {$ENDIF}

        FOwner.releaseSendRequest(pvSendRequest);
        self.RequestDisconnect;
      end;
    finally
      decReferenceCounter('TIocpClientContext.postSendRequest', pvSendRequest);
    end;
  end else
  begin    // lock fail
     FOwner.releaseSendRequest(pvSendRequest);
  end;
end;

procedure TIocpClientContext.PostWSARecvRequest;
begin
  FRecvRequest.PostRequest;
end;



function TIocpClientContext.PostWSASendRequest(buf: Pointer; len: Cardinal;
    pvCopyBuf: Boolean = true): Boolean;
var
  lvRequest:TIocpSendRequest;
begin
  if self.Active then
  begin
    lvRequest := getSendRequest;
    lvRequest.setBuffer(buf, len, pvCopyBuf);
    Result := postSendRequest(lvRequest);
  end else
  begin
    Result := false;
  end;
end;



procedure TIocpClientContext.SetDebugINfo(const Value: string);
begin
  FDebugINfo := Value;
end;

procedure TIocpClientContext.SetOwner(const Value: TIocpTcpServer);
begin
  FOwner := Value;
  FRecvRequest.FOwner := FOwner;
  {$IFDEF SOCKET_REUSE}
  FDisconnectExRequest.FOwner := FOwner;
  {$ENDIF}
end;

procedure TIocpClientContext.SetSocketState(pvState:TSocketState);
begin
  FSocketState := pvState;
//  if Assigned(FOnSocketStateChanged) then
//  begin
//    FOnSocketStateChanged(Self);
//  end;
end;

procedure TIocpClientContext.unLock;
begin
  FContextLocker.unLock;
end;


procedure TIocpTcpServer.AddToOnlineList(pvObject: TIocpClientContext);
{$IFDEF USE_HASHTABLE}
{$ELSE}
var
  lvHash:Integer;
{$ENDIF}
begin
  {$IFDEF USE_HASHTABLE}
    FLocker.lock('AddToOnlineList');
    try
      FOnlineContextList.Add(pvObject.FSocketHandle, pvObject);
    finally
      FLocker.unLock;
    end;
  {$ELSE}
    lvHash := pvObject.RawSocket.SocketHandle and SOCKET_HASH_SIZE;

    pvObject.FPreForHash := nil;

    FLocker.lock('AddToOnlineList');
    try
      pvObject.FNextForHash := FClientsHash[lvHash];
      if FClientsHash[lvHash] <> nil then
        FClientsHash[lvHash].FPreForHash := pvObject;
      FClientsHash[lvHash] := pvObject;
    finally
      FLocker.unLock;
    end;

    FOnlineContextList.add(pvObject);
  {$ENDIF}
end;

function TIocpTcpServer.checkClientContextValid(const pvClientContext: TIocpClientContext): Boolean;
begin
  Result := (pvClientContext.FOwner = Self);
end;

procedure TIocpTcpServer.close;
begin
  SetActive(False);
end;

constructor TIocpTcpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocker := TIocpLocker.Create('iocpTcpServer');
  FSafeLogger:=TSafeLogger.Create();
  FSafeLogger.setAppender(TLogFileAppender.Create(True));

  FKeepAlive := true;
  FContextPool := TBaseQueue.Create;
  FSendRequestPool := TBaseQueue.Create;
    
  FIocpEngine := TIocpEngine.Create();
  {$IFDEF USE_HASHTABLE}
  FOnlineContextList := TDHashTable.Create(10949);
  {$ELSE}
  FOnlineContextList := TContextDoublyLinked.Create();
  {$ENDIF}

  FListenSocket := TRawSocket.Create;

  FIocpAcceptorMgr := TIocpAcceptorMgr.Create(Self, FListenSocket);
  FIocpAcceptorMgr.FMaxRequest := 100;
  FIocpAcceptorMgr.FMinRequest := 1;

  // post wsaRecv block size
  FWSARecvBufferSize := 1024 * 4;

  FWSASendBufferSize := 1024 * 8;
end;

destructor TIocpTcpServer.Destroy;
begin
  FSafeLogger.Enable := false;

  FIsDestroying := true;

  safeStop;

  if FDataMoniter <> nil then FDataMoniter.Free;

  FContextPool.FreeDataObject;

  FSendRequestPool.FreeDataObject;

  FListenSocket.Free;
  FIocpAcceptorMgr.Free;
  FIocpEngine.Free;

  FOnlineContextList.Free;

  FContextPool.Free;
  FSendRequestPool.Free;


  FSafeLogger.Free;

  FLocker.Free;
  inherited Destroy;
end;



procedure TIocpTcpServer.DisconnectAll;
{$IFDEF USE_HASHTABLE}
var
  I:Integer;
  lvBucket: PDHashData;
  lvClientContext:TIocpClientContext;
{$ELSE}
var
  lvClientContext, lvNextContext:TIocpClientContext;
{$ENDIF}
begin
  {$IFDEF USE_HASHTABLE}
  FLocker.lock('DisconnectAll');
  try
    for I := 0 to FOnlineContextList.BucketSize - 1 do
    begin
      lvBucket := FOnlineContextList.Buckets[I];
      while lvBucket<>nil do
      begin
        lvClientContext := TIocpClientContext(lvBucket.Data);
        if lvClientContext <> nil then
        begin
          lvClientContext.RequestDisconnect;
        end;
        lvBucket:=lvBucket.Next;
      end;
    end;
  finally
    FLocker.unLock;
  end;
  {$ELSE}
  FOnlineContextList.FLocker.lock('DisconnectAll');
  try


    lvNextContext := FOnlineContextList.FHead;

    // request all context discounnt
    while lvNextContext <> nil do
    begin
      lvClientContext := lvNextContext;
      lvNextContext := lvNextContext.FNext;
      lvClientContext.RequestDisconnect;
    end;
  finally
    FOnlineContextList.FLocker.unLock;
  end;
  {$ENDIF}


end;


function TIocpTcpServer.logCanWrite: Boolean;
begin
  Result := (not isDestroying) and FSafeLogger.Enable;
end;


procedure TIocpTcpServer.DoAcceptExResponse(pvRequest: TIocpAcceptExRequest);

{$IFDEF SOCKET_REUSE}
var
  lvErrCode:Integer;
{$ELSE}
var
  lvRet:Integer;
  lvErrCode:Integer;
{$ENDIF}
  function DoAfterAcceptEx():Boolean;
  begin
    Result := true;
    if Assigned(FOnContextAccept) then
    begin
      FOnContextAccept(pvRequest.FClientContext.RawSocket.SocketHandle,
         pvRequest.FClientContext.RemoteAddr, pvRequest.FClientContext.RemotePort, Result);

      if not Result then
      begin
        {$IFDEF DEBUG_ON}
         if logCanWrite then
          FSafeLogger.logMessage('OnAcceptEvent vAllowAccept = false');
        {$ENDIF}
      end;
    end;
    if Result then
    begin
      if FKeepAlive then
      begin
        Result := pvRequest.FClientContext.FRawSocket.setKeepAliveOption();
        if not Result then
        begin
          lvErrCode := GetLastError;
          {$IFDEF DEBUG_ON}
           if logCanWrite then
            FSafeLogger.logMessage('FClientContext.FRawSocket.setKeepAliveOption, Error:%d', [lvErrCode]);
          {$ENDIF}
        end;
      end;
    end;

  end;
begin
  if pvRequest.FErrorCode = 0 then
  begin
    if DoAfterAcceptEx then
    begin
     {$IFDEF SOCKET_REUSE}
      pvRequest.FClientContext.DoConnected;
     {$ELSE}
      lvRet := FIocpEngine.IocpCore.bind2IOCPHandle(
         pvRequest.FClientContext.FRawSocket.SocketHandle, 0);
      if lvRet = 0 then
      begin     // binding error
        lvErrCode := GetLastError;

        {$IFDEF DEBUG_ON}
         if FOwner.logCanWrite then
         FOwner.FSafeLogger.logMessage(
            'bind2IOCPHandle(%d) in TIocpTcpServer.DoAcceptExResponse occur Error :%d',
            [FClientContext.FRawSocket.SocketHandle, lvErrCode]);
        {$ENDIF}

        DoClientContextError(pvRequest.FClientContext, lvErrCode);

        pvRequest.FClientContext.FRawSocket.close;

        // relase client context object
        releaseClientContext(pvRequest.FClientContext);
        pvRequest.FClientContext := nil;
      end else
      begin
        pvRequest.FClientContext.DoConnected;
      end;
      {$ENDIF}
    end else
    begin
     {$IFDEF SOCKET_REUSE}
      pvRequest.FClientContext.FRawSocket.ShutDown;

      // post disconnectEx
      pvRequest.FClientContext.FDisconnectExRequest.DirectlyPost;
      pvRequest.FClientContext := nil;
     {$ELSE}
      pvRequest.FClientContext.FRawSocket.close;

      // return to pool
      releaseClientContext(pvRequest.FClientContext);
      pvRequest.FClientContext := nil;
      {$ENDIF}
    end;
  end else
  begin
    // relase client context object
    releaseClientContext(pvRequest.FClientContext);
    pvRequest.FClientContext := nil;
  end;

  // remove from list
  FIocpAcceptorMgr.removeRequestObject(pvRequest);

  if FActive then FIocpAcceptorMgr.checkPostRequest;
end;

procedure TIocpTcpServer.DoClientContextError(pvClientContext:
    TIocpClientContext; pvErrorCode: Integer);
begin
{$IFDEF DEBUG_ON}
  if logCanWrite then
    FSafeLogger.logMessage('TIocpTcpServer.DoClientContextError Error:%d',  [pvErrorCode], 'ContextError', lgvError);
{$ENDIF}

  if Assigned(FOnClientContextError) then
    FOnClientContextError(pvClientContext, pvErrorCode);
end;

procedure TIocpTcpServer.doReceiveData(pvIocpClientContext: TIocpClientContext;
  pvRequest: TIocpRecvRequest);
begin
  if Assigned(FOnDataReceived) then
    FOnDataReceived(pvIocpClientContext,
      pvRequest.FRecvBuffer.buf, pvRequest.FBytesTransferred,
      pvRequest.FErrorCode);
end;

function TIocpTcpServer.findContext(
  pvSocketHandle: TSocket): TIocpClientContext;
{$IFDEF USE_HASHTABLE}

{$ELSE}
var
  lvHash:Integer;
  lvObj:TIocpClientContext;
{$ENDIF}
begin
  FLocker.lock('findContext');
  try
    {$IFDEF USE_HASHTABLE}
    Result := TIocpClientContext(FOnlineContextList.FindFirstData(pvSocketHandle));
    {$ELSE}
    Result := nil;
    lvHash := pvSocketHandle and SOCKET_HASH_SIZE;
    lvObj := FClientsHash[lvHash];
    while lvObj <> nil do
    begin
      if lvObj.FRawSocket.SocketHandle = pvSocketHandle then
      begin
        Result := lvObj;
        break;
      end;
      lvObj := lvObj.FNextForHash;
    end;
    {$ENDIF}
  finally
    FLocker.unLock;
  end;
end;

function TIocpTcpServer.getClientContext: TIocpClientContext;
begin
  Result := TIocpClientContext(FContextPool.Pop);
  if Result = nil then
  begin
    if FClientContextClass <> nil then
    begin
      Result := FClientContextClass.Create;
      onCreateClientContext(Result);
    end else
    begin
      Result := TIocpClientContext.Create;
      onCreateClientContext(Result);
    end;
    if (FDataMoniter <> nil) then
    begin
      InterlockedIncrement(FDataMoniter.FContextCreateCounter);
    end;
    Result.FSendRequestLink.setMaxSize(FMaxSendingQueueSize);
  end;
  Result.FAlive := True;
  Result.DoCleanUp;
  Result.Owner := Self;
  if (FDataMoniter <> nil) then
  begin
    InterlockedIncrement(FDataMoniter.FContextOutCounter);
  end;
end;

function TIocpTcpServer.GetWorkerCount: Integer;
begin
  Result := FIocpEngine.WorkerCount;
end;

function TIocpTcpServer.isDestroying: Boolean;
begin
  Result := FIsDestroying or (csDestroying in self.ComponentState)
end;

procedure TIocpTcpServer.onCreateClientContext(
  const context: TIocpClientContext);
begin

end;

procedure TIocpTcpServer.open;
begin
  SetActive(true);
end;

procedure TIocpTcpServer.registerContextClass(pvContextClass: TIocpClientContextClass);
begin
  FClientContextClass := pvContextClass;
end;

procedure TIocpTcpServer.registerSendRequestClass(
  pvClass: TIocpSendRequestClass);
begin
  FIocpSendRequestClass := pvClass;
end;

function TIocpTcpServer.releaseClientContext(pvObject:TIocpClientContext):
    Boolean;
begin
  if lock_cmp_exchange(True, False, pvObject.FAlive) = true then
  begin
    pvObject.DoCleanUp;
    FContextPool.Push(pvObject);
    if (FDataMoniter <> nil) then
    begin
      InterlockedIncrement(FDataMoniter.FContextReturnCounter);
    end;
    Result := true;
  end else
  begin
    Result := false;
  end;
end;

function TIocpTcpServer.releaseSendRequest(pvObject:TIocpSendRequest): Boolean;
begin
  Assert(FSendRequestPool <> nil);
  if lock_cmp_exchange(True, False, pvObject.FAlive) = True then
  begin
    if (FDataMoniter <> nil) then
    begin
      InterlockedIncrement(FDataMoniter.FSendRequestReturnCounter);
    end;
    FSendRequestPool.Push(pvObject);
    Result := true;
  end else
  begin
    Result := false;
  end;
end;

procedure TIocpTcpServer.RemoveFromOnOnlineList(pvObject: TIocpClientContext);
{$IFDEF USE_HASHTABLE}
  {$IFDEF DEBUG_ON}
    var
      lvSucc:Boolean;
  {$ENDIF}
{$ELSE}
var
  lvHash:Integer;
{$ENDIF}
begin
{$IFDEF USE_HASHTABLE}
  FLocker.lock('RemoveFromOnOnlineList');
  try
    {$IFDEF DEBUG_ON}
    lvSucc := FOnlineContextList.DeleteFirst(pvObject.FSocketHandle);
    Assert(lvSucc);
    {$ELSE}
    FOnlineContextList.DeleteFirst(pvObject.FSocketHandle);
    {$ENDIF}                                               
  finally
    FLocker.unLock;
  end;
{$ELSE} 
  FOnlineContextList.remove(pvObject);

  FLocker.lock('RemoveFromOnOnlineList');
  try
    // hash
    if pvObject.FPreForHash <> nil then
    begin
      pvObject.FPreForHash.FNextForHash := pvObject.FNextForHash;
      if pvObject.FNextForHash <> nil then
        pvObject.FNextForHash.FPreForHash := pvObject.FPreForHash;
    end else
    begin     // first ele
      lvHash := pvObject.RawSocket.SocketHandle and SOCKET_HASH_SIZE;
      FClientsHash[lvHash] := pvObject.FNextForHash;
      if FClientsHash[lvHash] <> nil then
        FClientsHash[lvHash].FPreForHash := nil;
    end;
  finally
    FLocker.unLock;
  end;

  pvObject.FNextForHash := nil;
  pvObject.FPreForHash := nil;
{$ENDIF}

end;

procedure TIocpTcpServer.safeStop;
begin
  if FActive then
  begin
    FActive := false;

    // close listen socket
    FListenSocket.close;

    DisconnectAll;

    if not WaitForContext(10000) then
    begin
      Sleep(10);
    end;
    Sleep(10);
    // engine stop
    FIocpEngine.safeStop;

    //Assert(self.) 
  end; 
end;

procedure TIocpTcpServer.SetActive(pvActive:Boolean);
begin
  if pvActive <> FActive then
  begin
    if pvActive then
    begin
      if FDataMoniter <> nil then FDataMoniter.clear;
      
      // engine start
      FIocpEngine.checkStart;
      
      // create listen socket
      FListenSocket.createTcpOverlappedSocket;

      // bind to listen port
      if not FListenSocket.bind('', FPort) then
      begin
        RaiseLastOSError;
      end;

      // listen
      if not FListenSocket.listen() then
      begin
        RaiseLastOSError;
      end;

      FIocpEngine.IocpCore.bind2IOCPHandle(FListenSocket.SocketHandle, 0);

//
//      FIocpAcceptorMgr.FMinRequest := 10;
//      FIocpAcceptorMgr.FMaxRequest := 100;

      // post AcceptEx request
      FIocpAcceptorMgr.checkPostRequest;

      FActive := True;
    end else
    begin
      safeStop;
    end; 
  end;
end;

procedure TIocpTcpServer.setMaxSendingQueueSize(pvSize: Integer);
begin
  if pvSize <= 0 then
  begin
    FMaxSendingQueueSize := 10;
  end else
  begin
    FMaxSendingQueueSize := pvSize;
  end;
end;

procedure TIocpTcpServer.SetName(const NewName: TComponentName);
begin
  inherited;
{$IFDEF DEBUG_ON}
  if FSafeLogger.Appender is TLogFileAppender then
  begin
    if NewName <> '' then
    begin
      TLogFileAppender(FSafeLogger.Appender).FilePreFix := NewName + '_';
    end;
  end;
{$ENDIF}
end;

procedure TIocpTcpServer.SetWorkerCount(const Value: Integer);
begin
  FIocpEngine.setWorkerCount(Value);
end;

procedure TIocpTcpServer.createDataMonitor;
begin
  if FDataMoniter = nil then
  begin
    FDataMoniter := TIocpDataMonitor.Create;
  end;
end;

function TIocpTcpServer.GetClientCount: Integer;
begin
  Result := FOnlineContextList.Count;
end;

procedure TIocpTcpServer.getOnlineContextList(pvList:TList);
{$IFDEF USE_HASHTABLE}
var
  I:Integer;
  lvBucket: PDHashData;
{$ELSE}
{$ENDIF}
begin
  {$IFDEF USE_HASHTABLE}
  FLocker.lock('getOnlineContextList');
  try
    for I := 0 to FOnlineContextList.BucketSize - 1 do
    begin
      lvBucket := FOnlineContextList.Buckets[I];
      while lvBucket<>nil do
      begin
        if lvBucket.Data <> nil then
        begin
           pvList.Add(lvBucket.Data);
        end;
        lvBucket:=lvBucket.Next;
      end;
    end;
  finally
    FLocker.unLock;
  end;
  {$ELSE}
  FOnlineContextList.write2List(pvList);
  {$ENDIF}
end;

function TIocpTcpServer.getSendRequest: TIocpSendRequest;
begin
  if Self = nil then
  begin
    Result := nil;
    Exit;
  end;
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
    if (FDataMoniter <> nil) then
    begin
      InterlockedIncrement(FDataMoniter.FSendRequestCreateCounter);
    end;
  end;
  Result.FAlive := true;
  Result.DoCleanup;
  Result.FOwner := Self;
  if (FDataMoniter <> nil) then
  begin
    InterlockedIncrement(FDataMoniter.FSendRequestOutCounter);
  end;
end;

procedure TIocpTcpServer.SetWSARecvBufferSize(const Value: cardinal);
begin
  FWSARecvBufferSize := Value;
  if FWSARecvBufferSize = 0 then
  begin
    FWSARecvBufferSize := 1024 * 4;
  end;
end;

procedure TIocpTcpServer.SetWSASendBufferSize(const Value: cardinal);
begin
  FWSASendBufferSize := Value;
  if FWSASendBufferSize <=0 then
    FWSASendBufferSize := 1024 * 8;
end;

function TIocpTcpServer.WaitForContext(pvTimeOut: Cardinal): Boolean;
var
  l:Cardinal;
  c:Integer;
begin
  l := GetTickCount;
  c := FOnlineContextList.Count;
  while (c > 0) do
  begin
    {$IFDEF MSWINDOWS}
    SwitchToThread;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}

    if GetTickCount - l > pvTimeOut then
    begin
      {$IFDEF DEBUG_ON}
       if logCanWrite then
        FSafeLogger.logMessage('WaitForContext End Current num:%d', [c]);
      {$ENDIF}
      Break;
    end;
    c := FOnlineContextList.Count;
  end;

  Result := FOnlineContextList.Count = 0;  
end;

procedure TIocpAcceptorMgr.checkPostRequest;
var
  lvRequest:TIocpAcceptExRequest;
  i:Integer;
begin
  Assert(FOwner <> nil);
  FLocker.lock;
  try
    if FList.Count > FMinRequest then Exit;

    i := 0;
    // post request
    while FList.Count < FMaxRequest do
    begin
      lvRequest := TIocpAcceptExRequest.Create(FOwner);
      lvRequest.FClientContext := FOwner.getClientContext;
      lvRequest.FAcceptorMgr := self;
      if lvRequest.PostRequest then
      begin
        FList.Add(lvRequest);
        if (FOwner.FDataMoniter <> nil) then
        begin
          InterlockedIncrement(FOwner.FDataMoniter.FPostWSAAcceptExCounter);
        end;
      end else
      begin     // post fail
        inc(i);

        try
          // free this object instead return to pool
          lvRequest.FClientContext.FAlive := false;
          lvRequest.FClientContext.Free;
        except
        end;

        // return to pool
        //lvRequest.FClientContext.SetSocketState(ssDisconnected);
        //FOwner.releaseClientContext(lvRequest.FClientContext);

        // free request
        releaseRequestObject(lvRequest);
      end;

      if i > 100 then
      begin
         if FOwner.logCanWrite then
          FOwner.FSafeLogger.logMessage('TIocpAcceptorMgr.checkPostRequest errCounter:%d', [i], CORE_LOG_FILE);
         Break;
      end;
    end;
  finally
    FLocker.unLock;
  end;
end;

constructor TIocpAcceptorMgr.Create(AOwner: TIocpTcpServer; AListenSocket:
    TRawSocket);
begin
  inherited Create;
  FLocker := TIocpLocker.Create();
  FLocker.Name := 'acceptorLocker';
  FMaxRequest := 200;
  FMinRequest := 10;  
  FList := TList.Create;
  FOwner := AOwner;
  FListenSocket := AListenSocket;
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

constructor TIocpAcceptExRequest.Create(AOwner: TIocpTcpServer);
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

  FClientContext.FRemoteAddr := string(inet_ntoa(TSockAddrIn(remoteAddr^).sin_addr));
  FClientContext.FRemotePort := ntohs(TSockAddrIn(remoteAddr^).sin_port);
end;

procedure TIocpAcceptExRequest.HandleResponse;
begin
  Assert(FOwner <> nil);
  ///
  if (FOwner.FDataMoniter <> nil) then
  begin
    InterlockedIncrement(FOwner.FDataMoniter.FResponseWSAAcceptExCounter);
  end;

  if FErrorCode = 0 then
  begin
    // msdn
    // The socket sAcceptSocket does not inherit the properties of the socket
    //  associated with sListenSocket parameter until SO_UPDATE_ACCEPT_CONTEXT
    //  is set on the socket.
    FOwner.FListenSocket.UpdateAcceptContext(FClientContext.FRawSocket.SocketHandle);

    getPeerINfo();
  end;
  FOwner.DoAcceptExResponse(Self); 
end;

function TIocpAcceptExRequest.PostRequest: Boolean;
var
  dwBytes: Cardinal;
  lvRet:BOOL;
  lvErrCode:Integer;
  lp:POverlapped;
  {$IFDEF SOCKET_REUSE}
  lvRetCode:Integer;
  {$ENDIF}
begin
  {$IFDEF SOCKET_REUSE}
  if
    (FClientContext.FRawSocket.SocketHandle = INVALID_SOCKET)
    or
    (FClientContext.FRawSocket.SocketHandle = 0) then
  begin
    if (FOwner.FDataMoniter <> nil) then
      FOwner.FDataMoniter.incHandleCreateCounter;

    FClientContext.FRawSocket.createTcpOverlappedSocket;

    lvRetCode := FOwner.IocpEngine.IocpCore.bind2IOCPHandle(
      FClientContext.FRawSocket.SocketHandle, 0);
    if lvRetCode = 0 then
    begin     // binding error
      lvErrCode := GetLastError;
      if FOwner.logCanWrite then
       FOwner.FSafeLogger.logMessage(
        'bind2IOCPHandle(%d) in TIocpAcceptExRequest.PostRequest(SOCKET_REUSE) occur Error :%d',
        [FClientContext.FRawSocket.SocketHandle, lvErrCode], CORE_LOG_FILE);

      FClientContext.FRawSocket.close;
      if (FOwner.FDataMoniter <> nil) then
        FOwner.FDataMoniter.incHandleDestroyCounter;
      Result := false;
      Exit;
    end;
  end;
  {$ELSE}
  FClientContext.FRawSocket.createTcpOverlappedSocket;
  {$ENDIF}
  dwBytes := 0;
  lp := @FOverlapped;

  FClientContext.SetSocketState(ssAccepting);

  lvRet := IocpAcceptEx(FOwner.FListenSocket.SocketHandle
                , FClientContext.FRawSocket.SocketHandle
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
       if FOwner.logCanWrite then
        FOwner.FSafeLogger.logMessage('IocpAcceptEx(%d, %d) in TIocpAcceptExRequest.PostRequest occur Error:%d',
          [FOwner.FListenSocket.SocketHandle, FClientContext.FRawSocket.SocketHandle,
          lvErrCode], CORE_LOG_FILE);

      FOwner.DoClientContextError(FClientContext, lvErrCode);

      /// destroy socket
      FClientContext.RawSocket.close;
    end;
  end else
  begin
    Result := True;
  end;
end;

procedure TIocpAcceptExRequest.ResponseDone;
begin
  inherited;
  FAcceptorMgr.releaseRequestObject(Self);
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
  InterlockedDecrement(FOverlapped.refCount);
  if FOverlapped.refCount <> 0 then
    Assert(FOverlapped.refCount <>0);

  Assert(FOwner <> nil);
  try
    if (FOwner.FDataMoniter <> nil) then
    begin
      FOwner.FDataMoniter.incResponseWSARecvCounter;
      FOwner.FDataMoniter.incRecvdSize(FBytesTransferred);
    end;

    if not FOwner.Active then
    begin
      {$IFDEF DEBUG_ON}
       if FOwner.logCanWrite then
        FOwner.FSafeLogger.logMessage('IocpRecvRequest response server enginee is off');
      {$ENDIF}
    end else if FErrorCode <> 0 then
    begin
      {$IFDEF DEBUG_ON}
       if FOwner.logCanWrite then
        FOwner.FSafeLogger.logMessage('IocpRecvRequest response ErrorCode:%d',  [FErrorCode]);
      {$ENDIF}
      FOwner.DoClientContextError(FClientContext, FErrorCode);
      FClientContext.RequestDisconnect;
    end else if (FBytesTransferred = 0) then
    begin      // no data recvd, socket is break
      {$IFDEF DEBUG_ON}
        if FOwner.logCanWrite then
          FOwner.FSafeLogger.logMessage('IocpRecvRequest response FBytesTransferred is zero',  []);
      {$ENDIF}
      FClientContext.RequestDisconnect;
    end else
    begin
      FClientContext.DoReceiveData;
    end;
  finally
    if not FClientContext.FRequestDisconnect then
    begin
      FClientContext.PostWSARecvRequest;
    end;

    // may return to pool
    FClientContext.decReferenceCounter(
      Format('TIocpRecvRequest.WSARecvRequest.HandleResponse, debugInfo:%s, refcount:%d',
        [FDebugInfo,FOverlapped.refCount]), Self);                                     
  end;
end;

function TIocpRecvRequest.PostRequest(pvBuffer: PAnsiChar;
  len: Cardinal): Boolean;
var
  lvRet:Integer;
  lpNumberOfBytesRecvd: Cardinal;
begin
  Result := False;
  lpNumberOfBytesRecvd := 0;
  FRecvdFlag := 0;

  FRecvBuffer.buf := pvBuffer;
  FRecvBuffer.len := len;

  if FClientContext.incReferenceCounter('TIocpRecvRequest.WSARecvRequest.Post', Self) then
  begin
    {$IFDEF DEBUG_ON}
    InterlockedIncrement(FOverlapped.refCount);
    {$ENDIF}
    FDebugInfo := IntToStr(intPtr(FClientContext));
    lvRet := iocpWinsock2.WSARecv(FClientContext.FRawSocket.SocketHandle,
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
        {$IFDEF DEBUG_ON}
         if FOwner.logCanWrite then
          FOwner.FSafeLogger.logMessage('TIocpRecvRequest.PostRequest Error:%d',  [lvRet]);

        InterlockedDecrement(FOverlapped.refCount);
        {$ENDIF}


        // trigger error event
        FOwner.DoClientContextError(FClientContext, lvRet);

        // decReferenceCounter
        FClientContext.decReferenceCounterAndRequestDisconnect(
        'TIocpRecvRequest.WSARecvRequest.Error', Self);

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
  FClientContext := nil;
  FBuf := nil;
  FLen := 0;
  FPosition := 0;
end;

procedure TIocpSendRequest.HandleResponse;
var
  lvCompleted:Boolean;  // all buffer is sent, for release self
begin
  FCanGiveBack := False;
  
  FIsBusying := false;
  lvCompleted := true;   // default true
  try
    Assert(FOwner<> nil);
    if (FOwner.FDataMoniter <> nil) then
    begin                                                       
      FOwner.FDataMoniter.incSentSize(FBytesTransferred);
      FOwner.FDataMoniter.incResponseWSASendCounter;

    end;

    if FErrorCode <> 0 then
    begin
      {$IFDEF DEBUG_ON}
       if FOwner.logCanWrite then
        FOwner.FSafeLogger.logMessage('TIocpSendRequest.HandleResponse FErrorCode:%d',  [FErrorCode]);
      {$ENDIF}
      FOwner.DoClientContextError(FClientContext, FErrorCode);
      FClientContext.RequestDisconnect; 
    end else
    begin
      onSendRequestSucc;
      if isCompleted then    // is all buf send completed?
      begin
        if FOwner.FDataMoniter <> nil then
        begin
          FOwner.FDataMoniter.incResponseSendObjectCounter;
        end;

        if Assigned(FOnDataRequestCompleted) then
        begin
          FOnDataRequestCompleted(FClientContext, Self);
        end;

        FClientContext.DoSendRequestCompleted(Self);

        FClientContext.postNextSendRequest;

      end else
      begin
        lvCompleted := False;
        if not checkSendNextBlock then
        begin
          lvCompleted := True;  // exception send break;
          
          {$IFDEF DEBUG_ON}
           if FOwner.logCanWrite then
             FOwner.FSafeLogger.logMessage('TIocpSendRequest.checkSendNextBlock return false',  []);
          {$ENDIF}

          /// kick out the clientContext
          FClientContext.RequestDisconnect;
        end;
      end;
    end;
  finally
    FClientContext.decReferenceCounter('TIocpSendRequest.WSASendRequest.Response', Self);

    if lvCompleted then    //
    begin
      //in reponseDone return to pool
      FCanGiveBack := True;
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
  Result := false;

  if FClientContext.incReferenceCounter('TIocpSendRequest.WSASendRequest', Self) then
  begin
    FIsBusying := True;
    FBytesSize := len;
    FWSABuf.buf := buf;
    FWSABuf.len := len;
    dwFlag := 0;
    lpNumberOfBytesSent := 0;
    
    lvRet := WSASend(FClientContext.FRawSocket.SocketHandle,
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
        try
           FIsBusying := False;
        {$IFDEF DEBUG_ON}
           if FOwner.logCanWrite then
             FOwner.FSafeLogger.logMessage('TIocpSendRequest.WSASendRequest Error:%d',  [lvRet]);
        {$ENDIF}

           FOwner.DoClientContextError(FClientContext, lvRet);
        except  
          on e:Exception do
          begin    
            {$IFDEF DEBUG_ON}
               if FOwner.logCanWrite then
                 FOwner.FSafeLogger.logMessage('TIocpSendRequest.InnerPostRequest Exception:' + E.Message, '', lgvError);
            {$ENDIF}
          end;
        end;
        /// request kick out
        FClientContext.decReferenceCounterAndRequestDisconnect('TIocpSendRequest.InnerPostRequest.fail', Self);

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
  end else
  begin    // lock fail
    FOwner.releaseSendRequest(Self);
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

procedure TIocpSendRequest.ResponseDone;
begin
  inherited;
  if FCanGiveBack then
  begin
    FOwner.releaseSendRequest(Self);
  end; 
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

    FContextCreateCounter := 0;
    FPostWSASendCounter:=0;
    FResponseWSASendCounter:=0;

    FSendRequestCreateCounter := 0;
    FPostWSARecvCounter:=0;
    FResponseWSARecvCounter:=0;

    FPushSendQueueCounter := 0;
    FResponseSendObjectCounter := 0;

    //FPostWSAAcceptExCounter:=0;
    //FResponseWSAAcceptExCounter:=0;
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

{$IFDEF SOCKET_REUSE}
procedure TIocpDataMonitor.incHandleCreateCounter;
begin
  InterlockedIncrement(FHandleCreateCounter);
end;

procedure TIocpDataMonitor.incHandleDestroyCounter;
begin
  InterlockedIncrement(FHandleDestroyCounter);
end;
{$ENDIF}

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

procedure TContextDoublyLinked.add(pvContext: TIocpClientContext);
var
  lvTail:TIocpClientContext;
begin
  FLocker.lock;
  try
    lvTail := FTail;
    pvContext.FPre := lvTail;
    pvContext.FNext := nil;
    FTail := pvContext;
    if (lvTail = nil) then FHead := pvContext else lvTail.FNext := pvContext;

//        final Node<E> l = last;
//        final Node<E> newNode = new Node<>(l, e, null);
//        last = newNode;
//        if (l == null)
//            first = newNode;
//        else
//            l.next = newNode;
//        size++;
//        modCount++;


//    Inc(FCount);
//
//    if FHead = nil then
//    begin
//      FHead := pvContext;
//    end else
//    begin
//      if FTail = nil then
//      begin
//        FCount := FCount;
//      end;
//      FTail.FNext := pvContext;
//      pvContext.FPre := FTail;
//    end;
//
//    FTail := pvContext;
//    FTail.FNext := nil;
//
//    if FTail = nil then
//    begin
//      FCount := FCount;
//    end;

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

function TContextDoublyLinked.indexOf(pvContext: TIocpClientContext): Integer;
var
  lvObj:TIocpClientContext;
  i:Integer;
begin
  FLocker.lock();
  try
    Result := -1;
    lvObj := FHead;
    i := 0;
    while lvObj <> nil do
    begin
      if lvObj = pvContext then
      begin
        Result := i;
        Break;
      end;
      Inc(i);
      lvObj := lvObj.FNext;
    end;
  finally
    FLocker.unLock;
  end;
end;

function TContextDoublyLinked.Pop: TIocpClientContext;
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

function TContextDoublyLinked.remove(pvContext:TIocpClientContext): Boolean;
var
  p,n:TIocpClientContext;
begin
  {$IFDEF DEBUG_ON}
  Assert(pvContext <> nil);
  Assert(indexOf(pvContext) <> -1);
  {$ENDIF}
  p := pvContext.FPre;
  n := pvContext.FNext;
  FLocker.lock;
  try
    if p = nil then FHead := n
    else
    begin
      p.FNext := n;
      pvContext.FPre := nil;
    end;

    if n = nil then FTail := p
    else
    begin
      n.FPre := p;
      pvContext.FNext := nil;
    end;

    Dec(FCount);

    Result := true;
  finally
    FLocker.unLock;
  end;
end;

procedure TContextDoublyLinked.write2List(pvList: TList);
var
  lvItem:TIocpClientContext;
  j:Integer;
begin
  FLocker.lock;
  try
    lvItem := FHead;
    j := 0;
    while lvItem <> nil do
    begin
      pvList.Add(lvItem);
      lvItem := lvItem.FNext;
      Inc(j);
    end;
    Assert(j = SElf.Count);
  finally
    FLocker.unLock;
  end;
end;

{ TIocpDisconnectExRequest }


function TIocpDisconnectExRequest.DirectlyPost: Boolean;
var
  lvErrorCode:Integer;
begin
  Result := False;
  Result := IocpDisconnectEx(FContext.RawSocket.SocketHandle, @FOverlapped, TF_REUSE_SOCKET, 0);
  if not Result then
  begin
    lvErrorCode := WSAGetLastError;
    if lvErrorCode <> ERROR_IO_PENDING then
    begin
      // do normal close;
      FContext.RawSocket.close;
      {$IFDEF DEBUG_ON}
         if FOwner.logCanWrite then
           FOwner.FSafeLogger.logMessage('TIocpDisconnectExRequest.PostRequest Error:%d',  [lvErrorCode]);
      {$ENDIF}

      // context may return to pool
      FContext.decReferenceCounter(
        Format('TIocpDisconnectExRequest.PostRequest Error: %d', [lvErrorCode]), Self
        );
      Result := false;
    end else
    begin
      Result := true;
    end;
  end;
end;

function TIocpDisconnectExRequest.PostRequest: Boolean;
var
  lvErrorCode:Integer;
begin
  Result := False;

  if FContext.incReferenceCounter('TIocpDisconnectExRequest.PostRequest', Self) then
  begin
    Result := IocpDisconnectEx(FContext.RawSocket.SocketHandle, @FOverlapped, TF_REUSE_SOCKET, 0);
    if not Result then
    begin
      lvErrorCode := WSAGetLastError;
      if lvErrorCode <> ERROR_IO_PENDING then
      begin
        // do normal close;
        FContext.RawSocket.close;
        {$IFDEF DEBUG_ON}
           if FOwner.logCanWrite then
             FOwner.FSafeLogger.logMessage('TIocpDisconnectExRequest.PostRequest Error:%d',  [lvErrorCode]);
        {$ENDIF}

        // context may return to pool
        FContext.decReferenceCounter(
          Format('TIocpDisconnectExRequest.PostRequest Error: %d', [lvErrorCode]), Self
          );
        Result := false;
      end else
      begin
        Result := true;
      end;
    end;
  end;
end;

end.
