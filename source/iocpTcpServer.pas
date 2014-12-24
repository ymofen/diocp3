(*
 *	 Unit owner: D10.Mofen
 *         homePage: http://www.diocp.org
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *	 v3.0.1(2014-7-16 21:36:30)
 *     + first release
 *
 *
 *
 *   thanks qsl's suggestion
 *)
 
unit iocpTcpServer;

interface

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

{.$DEFINE SOCKET_REUSE}

{$DEFINE USE_HASHTABLE}

{$IF defined(FPC) or defined(VER170) or defined(VER180) or defined(VER190) or defined(VER200) or defined(VER210)}
  {$DEFINE HAVE_INLINE}
{$IFEND}


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
  CORE_DEBUG_FILE = 'diocp_core_debug';

type
  TIocpTcpServer = class;
  TIocpAcceptorMgr = class;
  TIocpClientContext = class;
  TIocpRecvRequest = class;
  TIocpSendRequest = class;
  TIocpDisconnectExRequest = class;

  TDataReleaseType = (dtNone, dtFreeMem, dtDispose);

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
    // current socket handle
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


  {$IFDEF SOCKET_REUSE}
    /// <summary>
    ///
    /// </summary>
    procedure OnDisconnectExResponse(pvObject:TObject);
  {$ENDIF}
  private
    FAlive:Boolean;
    
    FContextLocker: TIocpLocker;
    
    {$IFDEF USE_HASHTABLE}
    {$ELSE}
    // socket/context map
    FPreForHash:TIocpClientContext;
    FNextForHash:TIocpClientContext;
    {$ENDIF}


    // link
    FPre:TIocpClientContext;
    FNext:TIocpClientContext;

    /// <summary>
    ///  sending flag
    /// </summary>
    FSending: Boolean;

    FActive: Boolean;


    FcurrSendRequest:TIocpSendRequest;
    
    FData: Pointer;
    FContextDNA: Integer;

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
    ///    must single thread operator
    /// </summary>
    procedure checkNextSendRequest;

    /// <example>
    ///  sendRequest to pool
    /// </example>
    procedure checkReleaseRes;


    procedure SetOwner(const Value: TIocpTcpServer);


  protected

    
    FOwner: TIocpTcpServer;
    /// <summary>
    ///   recvRequest
    /// </summary>
    FRecvRequest:TIocpRecvRequest;

    /// <summary>
    ///   request recv data
    /// </summary>
    procedure PostWSARecvRequest();virtual;



    /// <summary>
    ///
    /// </summary>
    function GetSendRequest: TIocpSendRequest;

    /// <summary>
    ///   Give Back
    /// </summary>
    function ReleaseSendRequest(pvObject:TIocpSendRequest): Boolean;

    /// <summary>
    ///  1.post reqeust to sending queue,
    ///    return false if SendQueue Size is greater than maxSize,
    ///
    ///  2.check sending flag, start if sending is false
    /// </summary>
    function InnerPostSendRequestAndCheckStart(pvSendRequest:TIocpSendRequest): Boolean;

    procedure InnerCloseContext;

    /// <summary>
    ///   投递完成后，继续投递下一个请求,
    ///     只在HandleResponse中调用
    /// </summary>
    procedure PostNextSendRequest; virtual;


    procedure lock();{$IFDEF HAVE_INLINE} inline;{$ENDIF}
    procedure unLock();{$IFDEF HAVE_INLINE} inline;{$ENDIF}
  protected
    procedure DoConnected;

    procedure DoCleanUp;virtual;

    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); virtual;

    procedure OnDisconnected; virtual;

    procedure OnConnected; virtual;

    procedure SetSocketState(pvState:TSocketState); virtual;
  public

    function GetSendQueueSize: Integer;


    constructor Create; virtual;
    destructor Destroy; override;

    procedure DoDisconnect;
    /// <summary>
    ///   lock context avoid disconnect,
    ///     lock succ return false else return false( context request disconnect)
    /// </summary>
    function LockContext(pvDebugInfo: string; pvObj: TObject): Boolean;

    procedure RequestDisconnect(pvDebugInfo: string = ''; pvObj: TObject = nil);

    /// <summary>
    ///  post send request to iocp queue, if post successful return true.
    ///    if request is completed, will call DoSendRequestCompleted procedure
    /// </summary>
    function PostWSASendRequest(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean =
        true): Boolean; overload;

    /// <summary>
    ///  post send request to iocp queue, if post successful return true.
    ///    if request is completed, will call DoSendRequestCompleted procedure
    /// </summary>
    function PostWSASendRequest(buf: Pointer; len: Cardinal; pvBufReleaseType:
        TDataReleaseType): Boolean; overload;
    procedure unLockContext(pvDebugInfo: string; pvObj: TObject);


    property Active: Boolean read FActive;

    property Data: Pointer read FData write FData;

    property DebugINfo: string read FDebugINfo write SetDebugINfo;

    /// <summary>
    ///  连接时进行 +1
    /// </summary>
    property ContextDNA: Integer read FContextDNA;

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
    FCounter:Integer;
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
    FSendBufferReleaseType: TDataReleaseType;
    
    FMaxSize:Integer;
    
    // for singlelinked
    FNext:TIocpSendRequest;

    FIsBusying:Boolean;

    FAlive: Boolean;

    FBytesSize:Cardinal;

    // send buf record
    FWSABuf:TWsaBuf;


    FBuf:Pointer;
    FLen:Cardinal;

    FOwner: TIocpTcpServer;

    procedure CheckClearSendBuffer();
 protected
    FClientContext:TIocpClientContext;

    FOnDataRequestCompleted: TOnDataRequestCompleted;

    procedure UnBindingSendBuffer();
  protected
    /// 0:none, 1:succ, 2:completed, 3: has err, 4:owner is off
    FReponseState:Byte;
    
    /// <summary>
    ///   post send
    /// </summary>
    function ExecuteSend: Boolean; virtual;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;


    procedure ResponseDone; override;



    /// <summary>
    ///   give back to sendRequest ObjectPool
    /// </summary>
    procedure DoCleanUp;virtual;


    function GetStateINfo: String; override;
    
    /// <summary>
    ///   post send buffer to iocp queue
    ///    if post fail then return false
    /// </summary>
    function InnerPostRequest(buf: Pointer; len: Cardinal): Boolean;


  public
    constructor Create; virtual;

    destructor Destroy; override;

    /// <summary>
    ///   set buf inneed to send
    /// </summary>
    procedure SetBuffer(buf: Pointer; len: Cardinal; pvBufReleaseType: TDataReleaseType); overload;

    /// <summary>
    ///   set buf inneed to send
    /// </summary>
    procedure SetBuffer(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean = true); overload;

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
    // FOwner:TIocpTcpServer;
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
    
    // sendRequest pool
    FAcceptExRequestPool: TBaseQueue;

    FList:TList;
    FListenSocket: TRawSocket;
    FLocker: TIocpLocker;
    FMaxRequest:Integer;
    FMinRequest:Integer;

  protected
  public
    constructor Create(AOwner: TIocpTcpServer; AListenSocket: TRawSocket);

    destructor Destroy; override;

    function GetRequestObject: TIocpAcceptExRequest;

    procedure ReleaseRequestObject(pvRequest:TIocpAcceptExRequest);

    procedure RemoveRequestObject(pvRequest:TIocpAcceptExRequest);

    procedure CheckPostRequest;

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
    FSendRequestAbortCounter :Integer;

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
    property SendRequestAbortCounter: Integer read FSendRequestAbortCounter;
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
  // thanks: 麦子仲肥19183455
  //  vcl for win64
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$IFEND}
  TIocpTcpServer = class(TComponent)
  private
    FContextDNA : Integer;
    
    FSafeLogger:TSafeLogger;

    FLocker:TIocpLocker;

    FMaxSendingQueueSize:Integer;

    FIsDestroying :Boolean;
    FWSARecvBufferSize: cardinal;
    procedure SetWSARecvBufferSize(const Value: cardinal);

    function isDestroying:Boolean;
    function logCanWrite:Boolean;
    
    function RequestContextDNA:Integer;
    
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
    function GetSendRequest: TIocpSendRequest;

    /// <summary>
    ///   push back to pool
    /// </summary>
    function ReleaseSendRequest(pvObject:TIocpSendRequest): Boolean;

  private
    procedure DoAcceptExResponse(pvRequest: TIocpAcceptExRequest);

    function GetClientCount: Integer;
  public
    procedure LogMessage(pvMsg: string; pvMsgType: string = ''; pvLevel: TLogLevel
        = lgvMessage); overload;
        
    procedure logMessage(pvMsg: string; const args: array of const; pvMsgType:
        string = ''; pvLevel: TLogLevel = lgvMessage); overload;


    constructor Create(AOwner: TComponent); override;

    procedure SetMaxSendingQueueSize(pvSize:Integer);

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
    procedure SafeStop;

    property Active: Boolean read FActive write SetActive;

    procedure open();

    procedure close;
    
    /// <summary>
    ///   
    /// </summary>
    function GetStateInfo: String;



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
    property MaxSendingQueueSize: Integer read FMaxSendingQueueSize;
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

uses
  DateUtils;


var
  __startTime:TDateTime;

const
  BytePerKB = 1024;
  BytePerMB = BytePerKB * 1024;
  BytePerGB = BytePerMB * 1024;

resourcestring
  strRecvZero      = '[%d]接收到0字节的数据,该连接将断开!';
  strRecvError     = '[%d]响应接收请求时出现了错误。错误代码:%d!';
  strRecvEngineOff = '[%d]响应接收请求时发现IOCP服务关闭';

  strSendEngineOff = '[%d]响应发送数据请求时发现IOCP服务关闭';
  strSendErr       = '[%d]响应发送数据请求时出现了错误。错误代码:%d!';
  strSendPostError = '[%d]投递发送数据请求时出现了错误。错误代码:%d';

  strBindingIocpError = '[%d]绑定到IOCP句柄时出现了异常, 错误代码:%d, (%s)';

  strPushFail      = '[%d]压入到待发送队列失败, 队列信息: %d/%d';






function GetRunTimeINfo: String;
var
  lvMSec, lvRemain:Int64;
  lvDay, lvHour, lvMin, lvSec:Integer;
begin
  lvMSec := MilliSecondsBetween(Now(), __startTime);
  lvDay := Trunc(lvMSec / MSecsPerDay);
  lvRemain := lvMSec mod MSecsPerDay;

  lvHour := Trunc(lvRemain / (MSecsPerSec * 60 * 60));
  lvRemain := lvRemain mod (MSecsPerSec * 60 * 60);

  lvMin := Trunc(lvRemain / (MSecsPerSec * 60));
  lvRemain := lvRemain mod (MSecsPerSec * 60);

  lvSec := Trunc(lvRemain / (MSecsPerSec));

  if lvDay > 0 then
    Result := Result + IntToStr(lvDay) + ' d ';

  if lvHour > 0 then
    Result := Result + IntToStr(lvHour) + ' h ';

  if lvMin > 0 then
    Result := Result + IntToStr(lvMin) + ' m ';

  if lvSec > 0 then
    Result := Result + IntToStr(lvSec) + ' s ';
end;


///TRunTimeINfoTools
function TransByteSize(pvByte: Int64): String;
var
  lvTB, lvGB, lvMB, lvKB:Word;
  lvRemain:Int64;
begin
  lvRemain := pvByte;

  lvTB := Trunc(lvRemain/BytePerGB/1024);
  //lvRemain := pvByte - (lvTB * BytePerGB * 1024);
  
  lvGB := Trunc(lvRemain/BytePerGB);

  lvGB := lvGB mod 1024;      // trunc TB

  lvRemain := lvRemain mod BytePerGB;

  lvMB := Trunc(lvRemain/BytePerMB);
  lvRemain := lvRemain mod BytePerMB;

  lvKB := Trunc(lvRemain/BytePerKB);
  lvRemain := lvRemain mod BytePerKB;
  Result := Format('%d TB, %d GB, %d MB, %d KB, %d B', [lvTB, lvGB, lvMB, lvKB, lvRemain]);
end;


  

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
    FOwner.FSafeLogger.logMessage('InnerCloseContext FReferenceCounter:%d', [FReferenceCounter],
    CORE_LOG_FILE);
  if not FActive then
  begin
    if FOwner.logCanWrite then
      FOwner.FSafeLogger.logMessage('InnerCloseContext FActive is false', CORE_LOG_FILE);
    exit;
  end;
{$ENDIF}
  if not FActive then exit;

//  Assert(FReferenceCounter = 0);
//  Assert(FActive);
  try
    FActive := false;
  {$IFDEF SOCKET_REUSE}

  {$ELSE}
    FRawSocket.close;
  {$ENDIF}

    checkReleaseRes;


    try
      if Assigned(FOwner.FOnClientContextDisconnected) then
      begin
        FOwner.FOnClientContextDisconnected(Self);
      end;
      OnDisconnected;
    except
    end;
  finally
    FOwner.RemoveFromOnOnlineList(Self);
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

  FContextLocker.lock();
  try
    lvRequest := TIocpSendRequest(FSendRequestLink.Pop);
    if lvRequest = nil then
    begin
      FSending := false;
      exit;
    end;
  finally
    FContextLocker.unLock;
  end;

  if lvRequest <> nil then
  begin   
    FcurrSendRequest := lvRequest;
    if lvRequest.ExecuteSend then
    begin
      if (FOwner.FDataMoniter <> nil) then
      begin
        FOwner.FDataMoniter.incPostSendObjectCounter;
      end;
    end else
    begin
      FcurrSendRequest := nil;

      /// cancel request
      lvRequest.CancelRequest;

      {$IFDEF DEBUG_ON}
      if FOwner.logCanWrite then
        FOwner.FSafeLogger.logMessage('TIocpClientContext.checkNextSendRequest.checkStart return false',  []);
      {$ENDIF}
      /// kick out the clientContext
      RequestDisconnect('checkNextSendRequest::lvRequest.checkSendNextBlock Fail', lvRequest);
    
      FOwner.releaseSendRequest(lvRequest);
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
      if (FOwner.FDataMoniter <> nil) then
      begin
        InterlockedIncrement(FOwner.FDataMoniter.FSendRequestAbortCounter);
      end;

      lvRequest.CancelRequest;
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
  FSendRequestLink := TIocpRequestSingleLink.Create(100);
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

    if FDebugStrings.Count > 40 then FDebugStrings.Delete(0);

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
  if self = nil then
  begin
    Assert(False);
  end;
  FContextLocker.lock('decReferenceCounter');
  Dec(FReferenceCounter);
  Result := FReferenceCounter;
  FDebugStrings.Add(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));

  if FDebugStrings.Count > 40 then
    FDebugStrings.Delete(0);

  if FReferenceCounter < 0 then
  begin
    if IsDebugMode then
    begin
      Assert(FReferenceCounter >=0);
    end else
    begin
      if FOwner.logCanWrite then
      begin
        FOwner.FSafeLogger.logMessage('TIocpClientContext.decReferenceCounter:%d, debugInfo:%s',
          [FReferenceCounter, FDebugStrings.Text], CORE_DEBUG_FILE);
      end;
      FReferenceCounter :=0;
    end;
  end;
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

{$IFDEF DEBUG_ON}
  if FOwner.logCanWrite then
    FOwner.FSafeLogger.logMessage('%d_RequestDisconnect:%s', [SocketHandle,pvDebugInfo],
      'RequestDisconnectDEBUG');
{$ENDIF}

  FRequestDisconnect := true;
  Dec(FReferenceCounter);
  FDebugStrings.Add(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));

  if FDebugStrings.Count > 40 then
    FDebugStrings.Delete(0);

  if FReferenceCounter < 0 then
  begin
    if IsDebugMode then
    begin
      Assert(FReferenceCounter >=0);
    end else
    begin
      if FOwner.logCanWrite then
      begin
        FOwner.FSafeLogger.logMessage('TIocpClientContext.decReferenceCounterAndRequestDisconnect:%d, debugInfo:%s',
          [FReferenceCounter, FDebugStrings.Text], CORE_DEBUG_FILE);
      end;
      FReferenceCounter :=0;
    end;
  end;
  if FReferenceCounter = 0 then
    lvCloseContext := true;
    
  FContextLocker.unLock; 
  
  if lvCloseContext then InnerCloseContext;
end;

function TIocpClientContext.ReleaseSendRequest(
  pvObject: TIocpSendRequest): Boolean;
begin
  Result := FOwner.releaseSendRequest(pvObject);
end;

procedure TIocpClientContext.RequestDisconnect(pvDebugInfo: string = ''; pvObj:
    TObject = nil);
var
  lvCloseContext:Boolean;
begin
  if not FActive then exit;

{$IFDEF DEBUG_ON}
  FOwner.logMessage('%d_RequestDisconnect:%s', [SocketHandle,pvDebugInfo],
      'RequestDisconnectDEBUG');
{$ENDIF}

  FContextLocker.lock('RequestDisconnect');
  if pvDebugInfo <> '' then
  begin
    FDebugStrings.Add(Format('*(%d):%d,%s', [FReferenceCounter, IntPtr(pvObj), pvDebugInfo]));
    if FDebugStrings.Count > 40 then FDebugStrings.Delete(0);
  end;
  {$IFDEF SOCKET_REUSE}
  lvCloseContext := False;
  if not FRequestDisconnect then
  begin
    // cancel
    FRawSocket.ShutDown();
    FRawSocket.CancelIO;

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
  if IsDebugMode then
  begin
    if FReferenceCounter <> 0 then
    begin
      Assert(FReferenceCounter = 0);
    end;

    if FSendRequestLink.Count > 0 then
    begin
      Assert(FSendRequestLink.Count = 0);
    end;
  end;

  FRawSocket.close;
  FRawSocket.Free;

  FRecvRequest.Free;
  
  if IsDebugMode then
  begin
    Assert(FSendRequestLink.Count = 0);
  end;

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
  FRequestDisconnect := false;
  FSending := false;

  FDebugStrings.Add(Format('-(%d):%d,%s', [FReferenceCounter, IntPtr(Self), '-----DoCleanUp-----']));

  if IsDebugMode then
  begin
    Assert(FReferenceCounter = 0);
    Assert(not FActive);
  end;


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
      if IsDebugMode then
      begin
        Assert(not FActive);
      end;
      {$IFDEF DEBUG_ON}
        if FOwner.logCanWrite then
          FOwner.FSafeLogger.logMessage('on DoConnected event is already actived', CORE_DEBUG_FILE);
      {$ENDIF}
    end else
    begin
      FContextDNA := FOwner.RequestContextDNA;
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

function TIocpClientContext.GetSendQueueSize: Integer;
begin
  Result := FSendRequestLink.Count;
end;

function TIocpClientContext.GetSendRequest: TIocpSendRequest;
begin
  Result := FOwner.GetSendRequest;
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

procedure TIocpClientContext.PostNextSendRequest;
begin
  checkNextSendRequest;
end;

function TIocpClientContext.InnerPostSendRequestAndCheckStart(
    pvSendRequest:TIocpSendRequest): Boolean;
var
  lvStart:Boolean;
begin
  lvStart := false;
  FContextLocker.lock();
  try
    Result := FSendRequestLink.Push(pvSendRequest);
    if Result then
    begin
      if not FSending then
      begin
        FSending := true;
        lvStart := true;  // start send work
      end;
    end;
  finally
    FContextLocker.unLock;
  end;

  {$IFDEF DEBUG_ON}
  if not Result then
  begin
    FOwner.logMessage(
      strPushFail, [FSocketHandle, FSendRequestLink.Count, FSendRequestLink.MaxSize]);
  end;
  {$ENDIF}

  if lvStart then
  begin      // start send work
    if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
    begin
      FOwner.FDataMoniter.incPushSendQueueCounter;
    end;
    CheckNextSendRequest;
  end;
end;

procedure TIocpClientContext.PostWSARecvRequest;
begin
  FRecvRequest.PostRequest;
end;



function TIocpClientContext.PostWSASendRequest(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean = true): Boolean;
var
  lvBuf: PAnsiChar;
begin
  if len = 0 then raise Exception.Create('PostWSASendRequest::request buf is zero!');
  if pvCopyBuf then
  begin
    GetMem(lvBuf, len);
    Move(buf^, lvBuf^, len);
    Result := PostWSASendRequest(lvBuf, len, dtFreeMem);
    if not Result then
    begin            //post fail
      FreeMem(lvBuf);
    end;
  end else
  begin
    lvBuf := buf;
    Result := PostWSASendRequest(lvBuf, len, dtNone);
  end;

end;

function TIocpClientContext.PostWSASendRequest(buf: Pointer; len: Cardinal;
    pvBufReleaseType: TDataReleaseType): Boolean;
var
  lvRequest:TIocpSendRequest;
begin
  Result := false;
  if len = 0 then raise Exception.Create('PostWSASendRequest::request buf is zero!');
  if self.Active then
  begin
    if self.incReferenceCounter('PostWSASendRequest', Self) then
    begin
      try
        lvRequest := GetSendRequest;
        lvRequest.SetBuffer(buf, len, pvBufReleaseType);
        Result := InnerPostSendRequestAndCheckStart(lvRequest);
        if not Result then
        begin
          /// Push Fail unbinding buf
          lvRequest.UnBindingSendBuffer;
          Self.RequestDisconnect('TIocpClientContext.PostWSASendRequest Post Fail',
            lvRequest);
          FOwner.ReleaseSendRequest(lvRequest);
        end;
      finally
        self.decReferenceCounter('PostWSASendRequest', Self);
      end;
    end;
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
  FContextDNA := 0;
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

  FMaxSendingQueueSize := 100;

  FIocpAcceptorMgr := TIocpAcceptorMgr.Create(Self, FListenSocket);
  FIocpAcceptorMgr.FMaxRequest := 100;
  FIocpAcceptorMgr.FMinRequest := 30;

  // post wsaRecv block size
  FWSARecvBufferSize := 1024 * 4;
end;

destructor TIocpTcpServer.Destroy;
begin
  FSafeLogger.Enable := false;

  FIsDestroying := true;

  SafeStop;

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


procedure TIocpTcpServer.LogMessage(pvMsg: string; const args: array of const;
  pvMsgType: string; pvLevel: TLogLevel);
begin
  if logCanWrite then
  begin
    FSafeLogger.logMessage(pvMsg, args, pvMsgType, pvLevel);
  end;  
end;

procedure TIocpTcpServer.LogMessage(pvMsg: string; pvMsgType: string = '';
    pvLevel: TLogLevel = lgvMessage);
begin
  if logCanWrite then
  begin
    FSafeLogger.logMessage(pvMsg, pvMsgType, pvLevel);
  end;
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
         if logCanWrite then
         FSafeLogger.logMessage(
            'bind2IOCPHandle(%d) in TIocpTcpServer.DoAcceptExResponse occur Error :%d',
            [pvRequest.FClientContext.RawSocket.SocketHandle, lvErrCode]);
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

function TIocpTcpServer.ReleaseSendRequest(pvObject:TIocpSendRequest): Boolean;
begin
  if self = nil then
  begin
    Assert(False);
  end;
  if FSendRequestPool = nil then
  begin
    // check call stack is crash
    Assert(FSendRequestPool <> nil);
  end;

  if IsDebugMode then
  begin
    Assert(pvObject.FAlive)
  end;

  if lock_cmp_exchange(True, False, pvObject.FAlive) = True then
  begin
    if (FDataMoniter <> nil) then
    begin
      InterlockedIncrement(FDataMoniter.FSendRequestReturnCounter);
    end;
    pvObject.DoCleanUp;
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

function TIocpTcpServer.RequestContextDNA: Integer;
begin
  Result := InterlockedIncrement(FContextDNA);
end;

procedure TIocpTcpServer.SafeStop;
begin
  if FActive then
  begin
    FActive := false;

    // close listen socket
    FListenSocket.close;

    DisconnectAll;

    if not WaitForContext(120000) then
    begin  // wait time out
      Sleep(10);

      // stop workers 10's
      if not FIocpEngine.StopWorkers(10000) then
      begin        // record info
        SafeWriteFileMsg('EngineWorkerInfo:' +
           sLineBreak + FIocpEngine.GetStateINfo + sLineBreak +
           '================================================' + sLineBreak +
           'TcpServerInfo:' +
           sLineBreak + GetStateINfo, Self.Name + '_SafeStopTimeOut');
      end;

    end else
    begin    // all context is give back to pool
      if not FIocpEngine.StopWorkers(120000) then
      begin        // record info
        SafeWriteFileMsg('EngineWorkerInfo:' +
           sLineBreak + FIocpEngine.GetStateINfo + sLineBreak +
           '================================================' + sLineBreak +
           'TcpServerInfo:' +
           sLineBreak + GetStateINfo, Self.Name + '_SafeStopTimeOut');
      end;
    end;

    // engine stop
    FIocpEngine.SafeStop();
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
      SafeStop;
    end;
  end;
end;

procedure TIocpTcpServer.SetMaxSendingQueueSize(pvSize:Integer);
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

function TIocpTcpServer.GetSendRequest: TIocpSendRequest;
begin
  if Self = nil then
  begin
    if IsDebugMode then
    begin
      Assert(Self <> nil)
    end;
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
  //Result.DoCleanup;
  Result.FOwner := Self;
  if (FDataMoniter <> nil) then
  begin
    InterlockedIncrement(FDataMoniter.FSendRequestOutCounter);
  end;
end;

function TIocpTcpServer.GetStateInfo: String;
var
  lvStrings:TStrings;
begin
  Result := '';
  if FDataMoniter = nil then exit;
  lvStrings := TStringList.Create;
  try
    if Active then
    begin
      lvStrings.Add('running');
    end else
    begin
      lvStrings.Add('stop');
    end;


    lvStrings.Add(Format('RecvInfo: post:%d, response:%d, remain:%d',
       [
         DataMoniter.PostWSARecvCounter,
         DataMoniter.ResponseWSARecvCounter,
         DataMoniter.PostWSARecvCounter -
         DataMoniter.ResponseWSARecvCounter
       ]
      ));

    lvStrings.Add('Recv size:' +
       TransByteSize(DataMoniter.RecvSize));


    //  Format('post:%d, response:%d, recvd:%d',
    //     [
    //       FIocpTcpServer.DataMoniter.PostWSARecvCounter,
    //       FIocpTcpServer.DataMoniter.ResponseWSARecvCounter,
    //       FIocpTcpServer.DataMoniter.RecvSize
    //     ]
    //    );

    lvStrings.Add(Format('SendInfo: post:%d, response:%d, remain:%d',
       [
         DataMoniter.PostWSASendCounter,
         DataMoniter.ResponseWSASendCounter,
         DataMoniter.PostWSASendCounter -
         DataMoniter.ResponseWSASendCounter
       ]
      ));

    lvStrings.Add(Format('SendRequest: create:%d, out:%d, return:%d',
       [
         DataMoniter.SendRequestCreateCounter,
         DataMoniter.SendRequestOutCounter,
         DataMoniter.SendRequestReturnCounter
       ]
      ));

    lvStrings.Add(Format('SendQueue: push/pop/complted/abort:%d, %d, %d, %d',
       [
         DataMoniter.PushSendQueueCounter,
         DataMoniter.PostSendObjectCounter,
         DataMoniter.ResponseSendObjectCounter,
         DataMoniter.SendRequestAbortCounter
       ]
      ));
      
    lvStrings.Add('Send Size:' +
       transByteSize(DataMoniter.SentSize));


    lvStrings.Add(Format('AcceptExInfo: post:%d, response:%d',
       [
         DataMoniter.PostWSAAcceptExCounter,
         DataMoniter.ResponseWSAAcceptExCounter
       ]
      ));

    lvStrings.Add(Format('SocketHandleInfo: create:%d, destroy:%d',
       [
         DataMoniter.HandleCreateCounter,
         DataMoniter.HandleDestroyCounter
       ]
      ));

    lvStrings.Add(Format('contextInfo: create:%d, out:%d, return:%d',
       [
         DataMoniter.ContextCreateCounter,
         DataMoniter.ContextOutCounter,
         DataMoniter.ContextReturnCounter
       ]
      ));

    lvStrings.Add(Format('online count: %d', [ClientCount]));
  
    lvStrings.Add(Format('worker count: %d', [WorkerCount]));

    lvStrings.Add('run info:' + GetRunTimeINfo);

    Result := lvStrings.Text;
  finally
    lvStrings.Free;

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
        FSafeLogger.logMessage('WaitForContext End Current Online num:%d', [c], CORE_LOG_FILE);
      {$ENDIF}
      Break;
    end;
    c := FOnlineContextList.Count;
  end;

  Result := FOnlineContextList.Count = 0;  
end;

procedure TIocpAcceptorMgr.CheckPostRequest;
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
      lvRequest := GetRequestObject;
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
          FOwner.FSafeLogger.logMessage('TIocpAcceptorMgr.CheckPostRequest errCounter:%d', [i], CORE_LOG_FILE);
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

  FAcceptExRequestPool := TBaseQueue.Create;
end;

destructor TIocpAcceptorMgr.Destroy;
begin
  FAcceptExRequestPool.FreeDataObject;
  FList.Free;
  FLocker.Free;
  FAcceptExRequestPool.Free;
  inherited Destroy;
end;

function TIocpAcceptorMgr.GetRequestObject: TIocpAcceptExRequest;
begin
  Result := TIocpAcceptExRequest(FAcceptExRequestPool.Pop);
  if Result = nil then
  begin
    Result := TIocpAcceptExRequest.Create(FOwner);
  end;
end;

procedure TIocpAcceptorMgr.ReleaseRequestObject(pvRequest:TIocpAcceptExRequest);
begin
  FAcceptExRequestPool.Push(pvRequest);
end;

procedure TIocpAcceptorMgr.RemoveRequestObject(pvRequest:TIocpAcceptExRequest);
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
      FOwner.logMessage(
         Format(strBindingIocpError,
           [FClientContext.FRawSocket.SocketHandle, lvErrCode, 'TIocpAcceptExRequest.PostRequest(SOCKET_REUSE)'])
         , CORE_LOG_FILE);

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
      FOwner.logMessage(
         Format(strBindingIocpError,
           [FClientContext.FRawSocket.SocketHandle, lvErrCode, 'TIocpAcceptExRequest.PostRequest'])
         , CORE_LOG_FILE);

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
var
  lvDNACounter:Integer;
  lvDebugInfo:String;
  lvRefCount:Integer;
begin
  lvDNACounter := Self.FCounter;

  {$IFDEF DEBUG_ON}
  InterlockedDecrement(FOverlapped.refCount);
  if FOverlapped.refCount <> 0 then
  begin
    if IsDebugMode then
    begin
      Assert(FOverlapped.refCount <>0);
    end;
    if FOwner.logCanWrite then
    begin
      FOwner.FSafeLogger.logMessage('TIocpRecvRequest.HandleResponse refCount:%d',
        [FOverlapped.refCount], CORE_DEBUG_FILE);
    end;
  end;
  {$ENDIF}

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
        FOwner.FSafeLogger.logMessage(
          Format(strRecvEngineOff, [FClientContext.FSocketHandle])
        );
      {$ENDIF}
      // avoid postWSARecv
      FClientContext.RequestDisconnect(
        Format(strRecvEngineOff, [FClientContext.FSocketHandle])
        , Self);
    end else if FErrorCode <> 0 then
    begin
      {$IFDEF DEBUG_ON}
      FOwner.FSafeLogger.logMessage(
        Format(strRecvError, [FClientContext.FSocketHandle, FErrorCode])
        );
      {$ENDIF}
      FOwner.DoClientContextError(FClientContext, FErrorCode);
      FClientContext.RequestDisconnect(
        Format(strRecvError, [FClientContext.FSocketHandle, FErrorCode])
        ,  Self);
    end else if (FBytesTransferred = 0) then
    begin      // no data recvd, socket is break
      {$IFDEF DEBUG_ON}
      FOwner.logMessage(strRecvZero,  [FClientContext.FSocketHandle]);
      {$ENDIF}
      FClientContext.RequestDisconnect(
        Format(strRecvZero,  [FClientContext.FSocketHandle]),  Self);
    end else
    begin
      FClientContext.DoReceiveData;
    end;
  finally
    lvDebugInfo := FDebugInfo;
    lvRefCount := FOverlapped.refCount;
    
    // postWSARecv before decReferenceCounter
    if not FClientContext.FRequestDisconnect then
    begin
      FClientContext.PostWSARecvRequest;
    end;

    // may return to pool
    FClientContext.decReferenceCounter(
      Format('TIocpRecvRequest.WSARecvRequest.HandleResponse, DNACounter:%d, debugInfo:%s, refcount:%d',
        [lvDNACounter, lvDebugInfo, lvRefCount]), Self);

//  for debug context DebugStrings
//    if FClientContext.FRequestDisconnect then
//    begin
//      lvBreak := true;
//    end else
//    begin
//      lvBreak := False
//    end;
//    // may return to pool
//    FClientContext.decReferenceCounter(
//      Format('TIocpRecvRequest.WSARecvRequest.HandleResponse, DNACounter:%d, debugInfo:%s, refcount:%d',
//        [lvDNACounter, FDebugInfo, FOverlapped.refCount]), Self);
//    if lvBreak then
//    begin
//      FClientContext.PostWSARecvRequest;
//    end;

  end;
end;

function TIocpRecvRequest.PostRequest(pvBuffer: PAnsiChar;
  len: Cardinal): Boolean;
var
  lvRet, lvDNACounter:Integer;
  lpNumberOfBytesRecvd: Cardinal;
begin
  Result := False;
  lpNumberOfBytesRecvd := 0;
  FRecvdFlag := 0;

  FRecvBuffer.buf := pvBuffer;
  FRecvBuffer.len := len;
  lvDNACounter := InterlockedIncrement(FCounter);
  if FClientContext.incReferenceCounter(Format(
    'TIocpRecvRequest.WSARecvRequest.Post, DNACounter:%d', [lvDNACounter]), Self) then
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

function TIocpSendRequest.ExecuteSend: Boolean;
begin
  Result := InnerPostRequest(FBuf, FLen);
end;

procedure TIocpSendRequest.CheckClearSendBuffer;
begin
  if FLen > 0 then
  begin
    case FSendBufferReleaseType of
      dtDispose: Dispose(FBuf);
      dtFreeMem: FreeMem(FBuf);
    end;
  end;
  FSendBufferReleaseType := dtNone;
  FLen := 0;
end;

constructor TIocpSendRequest.Create;
begin
  inherited Create;
end;

destructor TIocpSendRequest.Destroy;
begin
  CheckClearSendBuffer;
  inherited Destroy;
end;

procedure TIocpSendRequest.DoCleanUp;
begin
  CheckClearSendBuffer;
  FBytesSize := 0;
  FNext := nil;
  FOwner := nil;
  FClientContext := nil;
  FReponseState := 0;


  //FMaxSize := 0;
end;

procedure TIocpSendRequest.HandleResponse;
var
  lvContext:TIocpClientContext;
begin
  lvContext := FClientContext;
  FIsBusying := false;
  try
    Assert(FOwner<> nil);
    if (FOwner.FDataMoniter <> nil) then
    begin                                                       
      FOwner.FDataMoniter.incSentSize(FBytesTransferred);
      FOwner.FDataMoniter.incResponseWSASendCounter;
    end;
    if not FOwner.Active then
    begin
      FReponseState := 4;
      {$IFDEF DEBUG_ON}
       if FOwner.logCanWrite then
        FOwner.FSafeLogger.logMessage(
          Format(strSendEngineOff, [FClientContext.FSocketHandle])
          );
      {$ENDIF}
      // avoid postWSARecv
      FClientContext.RequestDisconnect(
        Format(strSendEngineOff, [FClientContext.FSocketHandle])
        , Self);
    end else if FErrorCode <> 0 then
    begin
      FReponseState := 3;
      {$IFDEF DEBUG_ON}
       if FOwner.logCanWrite then
        FOwner.FSafeLogger.logMessage(
          Format(strSendEngineOff, [FClientContext.FSocketHandle, FErrorCode])
          );
      {$ENDIF}
      FOwner.DoClientContextError(FClientContext, FErrorCode);
      FClientContext.RequestDisconnect(
         Format(strSendEngineOff, [FClientContext.FSocketHandle, FErrorCode])
          , Self);
    end else
    begin
      FReponseState := 2;
      if FOwner.FDataMoniter <> nil then
      begin
        FOwner.FDataMoniter.incResponseSendObjectCounter;
      end;

      if Assigned(FOnDataRequestCompleted) then
      begin
        FOnDataRequestCompleted(FClientContext, Self);
      end;

      FClientContext.DoSendRequestCompleted(Self);

      FClientContext.PostNextSendRequest;
    end;
  finally
//    if FClientContext = nil then
//    begin
//      Assert(False);
//      FReponseState := lvResponseState;
//    end;
    lvContext.decReferenceCounter('TIocpSendRequest.WSASendRequest.Response', Self);
  end;
end;

function TIocpSendRequest.InnerPostRequest(buf: Pointer; len: Cardinal):
    Boolean;
var
  lvErrorCode, lvRet: Integer;
  dwFlag: Cardinal;
  lpNumberOfBytesSent:Cardinal;
  lvContext:TIocpClientContext;
  lvOwner:TIocpTcpServer;
begin
  Result := false;
  FIsBusying := True;
  FBytesSize := len;
  FWSABuf.buf := buf;
  FWSABuf.len := len;
  dwFlag := 0;
  lvErrorCode := 0;
  lpNumberOfBytesSent := 0;

  // maybe on HandleResonse and release self
  lvOwner := FOwner;

  lvContext := FClientContext;
  if lvContext.incReferenceCounter('InnerPostRequest::WSASend_Start', self) then
  try
    lvRet := WSASend(lvContext.FRawSocket.SocketHandle,
                      @FWSABuf,
                      1,
                      lpNumberOfBytesSent,
                      dwFlag,
                      LPWSAOVERLAPPED(@FOverlapped),   // d7 need to cast
                      nil
    );
    if lvRet = SOCKET_ERROR then
    begin
      lvErrorCode := WSAGetLastError;
      Result := lvErrorCode = WSA_IO_PENDING;
      if not Result then
      begin
       FIsBusying := False;
       {$IFDEF DEBUG_ON}
       lvOwner.logMessage(
         Format(strSendPostError, [lvContext.FSocketHandle, lvErrorCode])
         );
       {$ENDIF}
        /// request kick out
       lvContext.RequestDisconnect(
          Format(strSendPostError, [lvContext.FSocketHandle, lvErrorCode])
          , Self);
      end else
      begin      // maybe on HandleResonse and release self
        if (lvOwner <> nil) and (lvOwner.FDataMoniter <> nil) then
        begin
          lvOwner.FDataMoniter.incPostWSASendSize(len);
          lvOwner.FDataMoniter.incPostWSASendCounter;
        end;
      end;
    end else
    begin       // maybe on HandleResonse and release self
      Result := True;
      if (lvOwner <> nil) and (lvOwner.FDataMoniter <> nil) then
      begin
        lvOwner.FDataMoniter.incPostWSASendSize(len);
        lvOwner.FDataMoniter.incPostWSASendCounter;
      end;
    end;
  finally
    if not Result then
    begin      // post fail, dec ref, if post succ, response dec ref
      if IsDebugMode then
      begin
        Assert(lvContext = FClientContext);
      end;
      lvContext.decReferenceCounter(
        Format('InnerPostRequest::WSASend_Fail, ErrorCode:%d', [lvErrorCode])
         , Self);

    end;

    // if result is true, maybe on HandleResponse dispose and push back to pool

  end;
end;

procedure TIocpSendRequest.ResponseDone;
begin
  inherited;
  if FOwner = nil then
  begin
    if IsDebugMode then
    begin
      Assert(FOwner <> nil);
      Assert(Self.FAlive);
    end;
  end else
  begin
    FOwner.releaseSendRequest(Self);
  end;
end;

procedure TIocpSendRequest.SetBuffer(buf: Pointer; len: Cardinal;
  pvBufReleaseType: TDataReleaseType);
begin
  CheckClearSendBuffer;
  FBuf := buf;
  FLen := len;
  FSendBufferReleaseType := pvBufReleaseType;
end;

procedure TIocpSendRequest.SetBuffer(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean = true);
var
  lvBuf: PAnsiChar;
begin
  if pvCopyBuf then
  begin
    GetMem(lvBuf, len);
    Move(buf^, lvBuf^, len);
    SetBuffer(lvBuf, len, dtFreeMem);
  end else
  begin
    SetBuffer(buf, len, dtNone);
  end;

//
//  if pvCopyBuf then
//  begin
//    if FCopyBuf.len > 0 then FreeMem(FCopyBuf.buf);
//
//    FCopyBuf.len := len;
//    GetMem(FCopyBuf.buf, FCopyBuf.len);
//    Move(buf^, FCopyBuf.buf^, FCopyBuf.len);
//    FBuf := FCopyBuf.buf;
//    FLen := FCopyBuf.len;
//  end else
//  begin
//    FBuf := buf;
//    FLen := len;
//  end;
//  FPosition := 0;
end;

procedure TIocpSendRequest.UnBindingSendBuffer;
begin
  FBuf := nil;
  FLen := 0;
  FSendBufferReleaseType := dtNone;
end;

function TIocpSendRequest.GetStateINfo: String;
begin
  Result :=Format('%s %s', [Self.ClassName, self.Remark]);
  if FResponding then
  begin
    Result :=Result + sLineBreak + Format('start:%s, datalen:%d, max:%d',
      [FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondStartTime), FWSABuf.len, FMaxSize]);
  end else
  begin
    Result :=Result + sLineBreak + Format('start:%s, end:%s, datalen:%d, max:%d',
      [FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondStartTime),
        FormatDateTime('MM-dd hh:nn:ss.zzz', FRespondEndTime),
        FWSABuf.len, FMaxSize]);
  end;
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


initialization
  __startTime :=  Now();



end.
