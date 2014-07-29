(*
 *	 Unit owner: d10.天地弦
 *	       blog: http://www.cnblogs.com/dksoft
 *
 *	 v0.1(2014-7-16 21:36:30)
 *     + first release
 *)
unit iocpTcpClient;

interface

uses
  WinSock, SysUtils, iocpEngine, iocpWinsock2, iocpProtocol, iocpSocketUtils,
  Windows, Classes, iocpRawSocket;

const
  buf_size = 1024 * 50;

type
  TIocpTcpClient = class;

  TOnConnected = procedure(Sender:TObject; errCode:Integer) of object;

  TOnWorkError = procedure(Sender:TObject; errCode:Integer) of object;

  TOnDataRecvd = procedure(Sender:TObject; buf:Pointer; len:cardinal; errCode:Integer) of object;

  /// <summary>
  ///   WSARecv io request
  /// </summary>
  TIocpRecvRequest = class(TIocpRequest)
  private
    FBuffer: iocpWinsock2.TWsaBuf;
    FRecvdBufferSize: Cardinal;
    FRecvdFlag: Cardinal;
    FOwner: TIocpTcpClient;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;
    /// <summary>
    ///   post recv request to iocp queue
    /// </summary>
    function PostRequest: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   connectEx io request
  /// </summary>
  TIocpConnectExRequest = class(TIocpRequest)
  private
    FBytesSent: DWORD;
    FOwner: TIocpTcpClient;
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;

    /// <summary>
    ///   post connectEx request to iocp queue
    /// </summary>
    function PostRequest: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   WSASend io request
  /// </summary>
  TIocpSendRequest = class(TIocpRequest)
  private
    FWokeBufferSize: Cardinal;
    FOwner: TIocpTcpClient;
    
  protected
    /// <summary>
    ///   iocp reply request, run in iocp thread
    /// </summary>
    procedure HandleResponse; override;


    /// <summary>
    ///   post send buffer to iocp queue
    /// </summary>
    function postSendBufferRequest(buf: Pointer; len: Cardinal): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;



  TIocpTcpClient = class(TComponent)
  private
    FSocketState:TSocketState;
    
    FRecvRequest: TIocpRecvRequest;
    FConnectExRequest: TIocpConnectExRequest;
    FIocpSendRequest: TIocpSendRequest;
    FHost: String;
    FRawSocket: TRawSocket;
    FIOCPEngine: TIocpEngine;
    FOnConnected: TOnConnected;
    FOnDataRecvd: TOnDataRecvd;
    FOnError: TOnWorkError;
    FonSocketStateChanged: TNotifyEvent;
    FPort: Integer;


    /// <summary>
    ///   on recved data, run in iocp worker thread
    /// </summary>
    procedure DoRecvd();

    /// <summary>
    ///   on connected request response, run in iocp worker thread
    /// </summary>
    procedure DoConnected();

    /// <summary>
    ///   set socket current state;
    /// </summary>
    procedure setSocketState(pvState:TSocketState);

    procedure doError(pvErrorCode:Integer);
    function GetSocketState: TSocketState;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    function isActive:Boolean;

    /// <summary>
    ///    sync execute connect
    ///      ssDisconnected -> ssConnected/ssDisconnected
    /// </summary>
    procedure Connect;

    /// <summary>
    ///   async execute connect
    ///     ssDisconnected -> ssConnecting -> ssConnected/ssDisconnected
    /// </summary>
    procedure connectASync();


    /// <summary>
    ///   async send buffer
    ///     send to iocp queue
    /// </summary>
    function sendBufferAsync(pvBuffer:Pointer; pvLen:Cardinal): Boolean;

    procedure Disconnect;

  published

    property Host: String read FHost write FHost;

    property Port: Integer read FPort write FPort;

    /// <summary>
    ///   socket current state
    /// </summary>
    property SocketState: TSocketState read GetSocketState;


    /// <summary>
    ///  on state changed
    ///    occur in post request methods or iocp worker thread
    /// </summary>
    property onSocketStateChanged: TNotifyEvent read FonSocketStateChanged write
        FonSocketStateChanged;
    

    /// <summary>
    ///   invoke in iocp worker thread
    /// </summary>
    property OnConnected: TOnConnected read FOnConnected write FOnConnected;

    /// <summary>
    ///  data recv
    ///   invoke in iocp worker thread
    /// </summary>
    property OnDataRecvd: TOnDataRecvd read FOnDataRecvd write FOnDataRecvd;


    /// <summary>
    ///  on work error
    ///    occur in post request methods or iocp worker thread
    /// </summary>
    property OnError: TOnWorkError read FOnError write FOnError;
  end;


implementation

procedure TIocpTcpClient.Disconnect;
begin
  if not isActive then Exit;
  
  FRawSocket.close;
  FIOCPEngine.stop;
  setSocketState(ssDisconnected);
end;

procedure TIocpTcpClient.connectASync;
begin
  if isActive then Exit;
  
  if not FIOCPEngine.Active then FIOCPEngine.start;
  
  FRawSocket.createTcpOverlappedSocket;

  
  FIOCPEngine.IocpCore.bind2IOCPHandle(FRawSocket.SocketHandle, 0);
  
  FConnectExRequest.PostRequest;
end;

constructor TIocpTcpClient.Create(AOwner:TComponent);
begin
  inherited;
  FIOCPEngine := TIocpEngine.Create();
  FIOCPEngine.setWorkerCount(1);

  FRawSocket := TRawSocket.Create();

  FRecvRequest := TIocpRecvRequest.Create();
  FRecvRequest.FOverlapped.iocpRequest := FRecvRequest;
  FRecvRequest.FOwner := Self;

  FConnectExRequest := TIocpConnectExRequest.Create();
  FConnectExRequest.FOverlapped.iocpRequest := FConnectExRequest;
  FConnectExRequest.FOwner := Self;

  FIocpSendRequest := TIocpSendRequest.Create();
  FIocpSendRequest.FOverlapped.iocpRequest := FIocpSendRequest;
  FIocpSendRequest.FOwner := Self;                            
end;

destructor TIocpTcpClient.Destroy;
begin
  // disable repeat request
  setSocketState(ssDisconnected);
    
  FRawSocket.close;
  FIOCPEngine.stop;
  FRecvRequest.Free;
  FConnectExRequest.Free;
  FIocpSendRequest.Free;

  FRawSocket.Free;
  FIOCPEngine.Free;
  inherited Destroy;
end;

procedure TIocpTcpClient.DoConnected;
begin
  if FConnectExRequest.FErrorCode = 0 then
  begin
    setSocketState(ssConnected);
        
    // post recv request, start to recv data
    FRecvRequest.PostRequest;
  end else
  begin
    setSocketState(ssDisconnected);

    // trigger error event
    doError(FConnectExRequest.FErrorCode);
  end;

  if Assigned(FOnConnected) then
    FOnConnected(Self, FConnectExRequest.FErrorCode);
end;

procedure TIocpTcpClient.doError(pvErrorCode: Integer);
begin
  if Assigned(FOnError) then
    FOnError(self, pvErrorCode);
end;

procedure TIocpTcpClient.DoRecvd;
begin
  if Assigned(FOnDataRecvd) then
    FOnDataRecvd(Self, FRecvRequest.FBuffer.buf,
      FRecvRequest.FBytesTransferred, FRecvRequest.FErrorCode);


end;

function TIocpTcpClient.isActive: Boolean;
begin
  Result := (FSocketState = ssConnected);  
end;

procedure TIocpTcpClient.Connect;
var
  lvAddr:TSockAddrIn;
  lvRet:Integer;
begin
  if isActive then Exit;
  
  if not FIOCPEngine.Active then FIOCPEngine.start;

  FRawSocket.createTcpSocket;

  socketBind(FRawSocket.SocketHandle, '0.0.0.0', 0);

  
  lvAddr := getSocketAddr(FHost, FPort);
  lvRet := iocpWinsock2.connect(FRawSocket.SocketHandle,TSockAddr(lvAddr), sizeof(TSockAddrIn));
  if lvRet = SOCKET_ERROR then
    RaiseLastOSError;


  // post recv quest
  FRecvRequest.PostRequest; 
end;

function TIocpTcpClient.GetSocketState: TSocketState;
begin
  Result := FSocketState;
end;

function TIocpTcpClient.sendBufferAsync(pvBuffer:Pointer; pvLen:Cardinal):
    Boolean;
begin
  Result := FIocpSendRequest.postSendBufferRequest(pvBuffer, pvLen);
end;

procedure TIocpTcpClient.setSocketState(pvState: TSocketState);
begin
  FSocketState := pvState;
  if Assigned(FonSocketStateChanged) then
    FonSocketStateChanged(Self);  
end;

constructor TIocpRecvRequest.Create;
begin
  inherited Create;
  GetMem(FBuffer.buf, buf_size);
  FBuffer.len := buf_size;
end;

destructor TIocpRecvRequest.Destroy;
begin
  FreeMem(FBuffer.buf, buf_size);
  inherited Destroy;
end;

procedure TIocpRecvRequest.HandleResponse;
begin
  if FBytesTransferred = 0 then
  begin      // no data recvd, socket is break
    FOwner.Disconnect;
  end else
  begin
    try
      FOwner.DoRecvd;
    finally
      // post recv request
      FOwner.FRecvRequest.PostRequest;
    end;

  end;
end;

function TIocpRecvRequest.PostRequest: Boolean;
var
  lvRet:Integer;
begin
  FRecvdBufferSize := 0;
  FRecvdFlag := 0;
  FBuffer.len := buf_size;

  lvRet := iocpWinsock2.WSARecv(FOwner.FRawSocket.SocketHandle,
     @FBuffer,
     1,
     FRecvdBufferSize,
     FRecvdFlag,
     @FOverlapped,
     nil
     );
  if lvRet = SOCKET_ERROR then
  begin
    lvRet := WSAGetLastError;
    Result := lvRet = WSA_IO_PENDING;
    if not Result then
    begin
      // trigger error event
      FOwner.doError(lvRet);
    end;
  end else
  begin
    Result := True;
  end;
end;

constructor TIocpConnectExRequest.Create;
begin
  inherited Create;
end;

destructor TIocpConnectExRequest.Destroy;
begin
  inherited Destroy;
end;

procedure TIocpConnectExRequest.HandleResponse;
begin
  FOwner.DoConnected;
end;

function TIocpConnectExRequest.PostRequest: Boolean;
var
  lvSockAddrIn:TSockAddrIn;
  lvRet:BOOL;
  lvErrCode:Integer;
  lp:Pointer;
begin
  FOwner.setSocketState(ssConnecting);
  lvSockAddrIn := iocpSocketUtils.getSocketAddr(FOwner.FHost, FOwner.FPort);

  iocpSocketUtils.socketBind(FOwner.FRawSocket.SocketHandle, '0.0.0.0', 0);

  lp :=@FOverlapped;
  lvRet := iocpSocketUtils.IocpConnectEx(FOwner.FRawSocket.SocketHandle,
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
      FOwner.doError(lvErrCode);
      FOwner.setSocketState(ssDisconnected);
    end;
  end else
  begin
    Result := True;
  end;


end;

constructor TIocpSendRequest.Create;
begin
  inherited Create;
end;

destructor TIocpSendRequest.Destroy;
begin
  inherited Destroy;
end;

procedure TIocpSendRequest.HandleResponse;
begin
  //FOwner;
end;


function TIocpSendRequest.postSendBufferRequest(buf: Pointer; len: Cardinal):
    Boolean;
var
  lvRet: Integer;
  dwFlag: Cardinal;
  wsaBuffer: TWsaBuf;
begin
  wsaBuffer.buf := buf;
  wsaBuffer.len := len;
  dwFlag := 0;
  FWokeBufferSize := 0;
  lvRet := WSASend(FOwner.FRawSocket.SocketHandle,
                    @wsaBuffer,
                    1,
                    FWokeBufferSize,
                    dwFlag,
                    @FOverlapped,
                    nil
  );
  if lvRet = SOCKET_ERROR then
  begin
    lvRet := WSAGetLastError;
    Result := lvRet = WSA_IO_PENDING;
    if not Result then
    begin
       FOwner.doError(lvRet);
    end;
  end else
  begin
    Result := True;
  end;
end;

end.
