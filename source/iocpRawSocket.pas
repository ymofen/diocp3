unit iocpRawSocket;

interface

uses
  windows, SysUtils, iocpWinsock2;

const
  SIO_KEEPALIVE_VALS = IOC_IN or IOC_VENDOR or 4;

type
  TKeepAlive = record
    OnOff: Integer;
    KeepAliveTime: Integer;
    KeepAliveInterval: Integer;
  end;
  TTCP_KEEPALIVE = TKeepAlive;
  PTCP_KEEPALIVE = ^TKeepAlive;
  
  /// <summary>
  ///   raw socket object
  ///     thanks my friend(ryan)
  /// </summary>
  TRawSocket = class(TObject)
  private
    FSocketHandle: TSocket;
  public
    procedure close;    
    procedure createTcpSocket;

    /// <summary>
    ///   create socket handle for overlapped
    /// </summary>
    procedure createTcpOverlappedSocket;

    function bind(const pvAddr: string; pvPort: Integer): Boolean;
    function listen(const backlog: Integer = 0): Boolean;

    function Recv(var data; const len: Integer): Integer;
    function Send(const data; const len: Integer): Integer;

    /// <summary>
    ///   default 5000 check alive
    /// </summary>
    function setKeepAliveOption(pvKeepAliveTime: Integer = 5000): Boolean;

    function setNoDelayOption(pvOption:Boolean): Boolean;

    property SocketHandle: TSocket read FSocketHandle;
  end;

implementation

{ TRawSocket }

function TRawSocket.bind(const pvAddr: string; pvPort: Integer): Boolean;
var
  sockaddr: TSockAddrIn;
begin
  FillChar(sockaddr, SizeOf(sockaddr), 0);
  with sockaddr do
  begin
    sin_family := AF_INET;
    if pvAddr = '' then
    begin
      sin_addr.S_addr := inet_addr(PAnsichar(AnsiString('0.0.0.0')));
    end else
    begin
      sin_addr.S_addr := inet_addr(PAnsichar(AnsiString(pvAddr)));
    end;
    sin_port :=  htons(pvPort);
  end;
  Result := iocpWinsock2.bind(FSocketHandle, TSockAddr(sockaddr), SizeOf(sockaddr)) = 0;
end;

procedure TRawSocket.close;
var
  lvTempSocket: TSocket;
begin
  lvTempSocket := FSocketHandle;
  if lvTempSocket <> INVALID_SOCKET then
  begin
    FSocketHandle := INVALID_SOCKET;
    Windows.CancelIo(lvTempSocket);
    closesocket(lvTempSocket);
  end;
end;

procedure TRawSocket.createTcpOverlappedSocket;
begin
  FSocketHandle := WSASocket(AF_INET,SOCK_STREAM, IPPROTO_TCP, Nil, 0, WSA_FLAG_OVERLAPPED);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

procedure TRawSocket.createTcpSocket;
begin
  FSocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

function TRawSocket.listen(const backlog: Integer): Boolean;
var
  queueSize: Integer;
begin
  if backlog = 0 then
  begin
    queueSize := SOMAXCONN;
  end
  else begin
    queueSize := backlog;
  end;
  Result := iocpWinsock2.listen(FSocketHandle, queueSize) = 0;
end;

function TRawSocket.Recv(var data; const len: Integer): Integer;
begin
  Result := iocpWinsock2.recv(FSocketHandle, data, len, 0);
end;

function TRawSocket.Send(const data; const len: Integer): Integer;
begin
  Result := iocpWinsock2.Send(FSocketHandle, data, len, 0);
end;

function TRawSocket.setKeepAliveOption(pvKeepAliveTime: Integer = 5000):
    Boolean;
var
  Opt, insize, outsize: integer;
  outByte: DWORD;
  inKeepAlive, outKeepAlive: TTCP_KEEPALIVE;
begin
  Result := false;
  Opt := 1;
  if SetSockopt(FSocketHandle, SOL_SOCKET, SO_KEEPALIVE,
     @Opt, sizeof(Opt)) = SOCKET_ERROR then exit;

  inKeepAlive.OnOff := 1;
  
  inKeepAlive.KeepAliveTime := pvKeepAliveTime;

  inKeepAlive.KeepAliveInterval := 1;
  insize := sizeof(TTCP_KEEPALIVE);
  outsize := sizeof(TTCP_KEEPALIVE);

  if WSAIoctl(FSocketHandle,
     SIO_KEEPALIVE_VALS,
     @inKeepAlive, insize,
     @outKeepAlive,
    outsize, outByte, nil, nil) <> SOCKET_ERROR then
  begin
    Result := true;
  end;
end;

function TRawSocket.setNoDelayOption(pvOption:Boolean): Boolean;
var
  bNoDelay: BOOL;
begin
  bNoDelay := pvOption;
  Result := setsockopt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, @bNoDelay, SizeOf(bNoDelay)) <> SOCKET_ERROR;
end;

end.
