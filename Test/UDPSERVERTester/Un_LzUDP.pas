unit Un_LzUDP;

interface

uses
  Windows, Classes, SysUtils, WinSock, WinSock2;

const
  DEFAULT_SENDBUF_SIZE = 8192;
  DEFAULT_RECVBUF_SIZE = 8192;

type
  {代理服务器验证类型}
  TAuthenType = (atNone, atUserPass);

  TLzUDPRecvThread = class;
  TLzUDPSocket = class;

  TProxyInfo = record
    Enabled: Boolean; //是否使用代理
    Address: string; //代理服务器地址
    Port: Integer; //代理服务器端口号
    Username: string; //代理服务器验证用户名
    Password: string; //密码
  end;

  TUDPException = class(Exception);

  TPeerInfo = record
    PeerIP: longword;
    PeerPort: integer;
  end;

  { utInit: 初始化包括设置缓冲区 utSend:发送数据 utRecv: 接收数据 utClose:关闭Socket}
  TUDPErrorType = (utInit, utSend, utRecv, utClose);
  TUDPErrorEvent = procedure(Sender: TObject; ErrorType: TUDPErrorType;
    var ErrorCode: Integer) of object;

  { 读数据事件 }
  TUDPReadEvent = procedure(UDPSocket: TLzUDPSocket; const PeerInfo: TPeerInfo)
    of object;

  //主要的UDP类
  TLzUDPSocket = class(TObject)
  private
    FSocket: TSocket;
    FPort: integer;
    //错误处理事件
    FOnSocketError: TUDPErrorEvent;
    //读数据事件
    FOnDataRead: TUDPReadEvent;
    //发送和接受缓冲大小
    FSendBufSize: Integer;
    FRecvBufSize: Integer;
    //记录接受到数据的远程机器的信息
    FPeerInfo: TPeerInfo;
    //可以在这段时间进行一些客户清理工作,得到数据到达时间
    FTimeOut: Longword;
    FOnTimeOut: TThreadMethod;
    //判断是否打开了套接字
    FActive: Boolean;
    FBroadcast: Boolean;
    FProxyInfo: TProxyInfo;
    //使用代理时保持连接的Tcp Socket
    FTcpSocket: TSocket;
    //代理服务器上的Udp映射地址信息
    FUdpProxyAddr: TSockAddrIn;
    //得到和设置缓冲大小的函数
    function GetSendBufSize: Integer;
    function GetRecvBufSize: Integer;
    procedure SetSendBufSize(Value: Integer);
    procedure SetRecvBufSize(Value: Integer);
    procedure SetActive(Value: Boolean);
    procedure SetTimeOut(Value: Longword);
    function InitSocket: Boolean;
    procedure FreeSocket;
    procedure DoActive(Active: boolean);
    procedure DataReceive;
    //连接代理服务器
    function ConnectToProxy: Boolean;
    //Tcp握手
    function Handclasp(Socket: TSocket; AuthenType: TAuthenType): Boolean;
    //建立Udp映射通道
    function MapUdpChannel(Socket: TSocket): Boolean;
    //通过Proxy发送数据
    function SendByProxy(Socket: TSocket; var buf; len: Integer; RemoteIP:
      longword;
      RemotePort: Integer): Integer;
    //从Proxy接收数据
    function RecvByProxy(Socket: TSocket; var buf; len: Integer; RemoteIP:
      longword;
      RemotePort: Integer): Integer;
  protected
    FUdpRecvThread: TLzUDPRecvThread;
  public
    constructor Create;
    destructor Destroy; override;

    //发送缓冲区数据
    function SendBuf(var Buf; Size: Integer; IP: longword; Port: Integer):
      Boolean;
    //发送文本
    function SendText(Text: string; IP: longword; Port: integer): Boolean;
    //两个发送广播消息的函数
    function BroadcastBuf(var Buf; Size: Integer; Port: Integer): Boolean;
    function BroadcastText(Text: string; Port: Integer): Boolean;
    //接收函数
    function RecvBuf(var Buf; Size: Integer; IP: longword; Port: Integer):
      Integer;
    //接受到远程数据的Client信息
    property PeerInfo: TPeerInfo read FPeerInfo;
    //发送和接收缓冲区大小
    property SendBufSize: Integer read GetSendBufSize write SetSendBufSize;
    property RecvBufSize: Integer read GetRecvBufSize write SetRecvBufSize;
    //监听端口
    property Port: Integer read FPort write FPort;
    //等待数据超时间 默认是$FFFFFFFF;
    property TimeOut: DWORD read FTimeOut write SetTimeOut;
    //打开套接字
    property Active: Boolean read FActive write SetActive;
    //是否可以广播
    property EnableBroadcast: Boolean read FBroadcast write FBroadcast;
    //代理配置
    property ProxyInfo: TProxyInfo read FProxyInfo write FProxyInfo;
    //有数据到达的事件
    property OnDataRead: TUdpReadEvent read FOnDataRead write FOnDataRead;
    //套接字发生错误事件
    property OnSocketError: TUdpErrorEvent read FOnSocketError write
      FOnSocketError;
    //接受数据发生超时
    property OnTimeOut: TThreadMethod read FOnTimeOut write FOnTimeOut;
  end;

  TLzUDPRecvThread = class(TThread)
  private
    FSocket: TLzUDPSocket;
    FEvent: WSAEvent;
    //接受到数据的事件
    FOnDataRecv: TThreadMethod;
    procedure InitEvent;
    procedure FreeEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AUdpSocket: TLzUDPSocket);
    destructor Destroy; override;
    property OnDataRecv: TThreadMethod read FOnDataRecv write FOnDataRecv;
    procedure Stop;
  end;

implementation

{ TLzUDPSocket }

function TLzUDPSocket.BroadcastBuf(var Buf; Size, Port: Integer): Boolean;
var
  ret, ErrorCode: Integer;
  saRemote: TSockAddrIn;
begin
  Result := False;
  saRemote.sin_family := AF_INET;
  saRemote.sin_port := htons(Port);
  saRemote.sin_addr.S_addr := htonl(INADDR_BROADCAST);

  if FProxyInfo.Enabled then
    ret := SendByProxy(FSocket, Buf, Size, saRemote.sin_addr.S_addr,
      ntohs(saRemote.sin_port))
  else
    ret := sendto(FSocket, Buf, Size, 0, saRemote, SizeOf(saRemote));
    
  if ret = SOCKET_ERROR then
  begin
    ErrorCode := GetLastError;
    if ErrorCode <> WSAEWOULDBLOCK then
    begin
      if Assigned(FOnSocketError) then
        FOnSocketError(Self, utSend, ErrorCode);
      if ErrorCode <> 0 then
        raise TUDPException.CreateFmt('广播数据时出错。错误码是%d',
          [ErrorCode]);
    end;
  end
  else
    Result := True;
end;

function TLzUDPSocket.BroadcastText(Text: string; Port: Integer): Boolean;
begin
  Result := BroadcastBuf(Text[1], Length(Text), Port);
end;

constructor TLzUDPSocket.Create;
var
  WSAData: TWSAData;
begin
  FActive := False;
  FPort := 0;
  FillChar(FPeerInfo, SizeOf(TPeerInfo), 0);
  FSendBufSize := DEFAULT_SENDBUF_SIZE;
  FRecvBufSize := DEFAULT_RECVBUF_SIZE;
  FSocket := INVALID_SOCKET;
  FUdpRecvThread := nil;
  FTimeOut := $FFFFFFFF;
  FBroadcast := False;
  FTcpSocket := INVALID_SOCKET;
  if WSAStartup(MakeWord(2, 2), WSAData) <> 0 then
    raise TUDPException.Create('本程序需要WinSock2，该机器上的Socket版本太低!');
end;

destructor TLzUDPSocket.Destroy;
begin
  if FActive then
    DoActive(False);

  if FTcpSocket <> INVALID_SOCKET then
    closesocket(FTcpSocket);

  if WSACleanup <> 0 then
    MessageBox(0, 'Socket清理失败!', '错误', MB_OK + MB_ICONERROR);

  inherited Destroy;
end;

procedure TLzUDPSocket.DoActive(Active: boolean);
var
  ErrorCode: Integer;
begin
  if Active = True then
  begin
    if InitSocket then
    begin
      FActive := True;
      try
        SetSendBufSize(FSendBufSize);
        SetRecvBufSize(FRecvBufSize);
        FUdpRecvThread := TLzUDPRecvThread.Create(True, Self);
        FUdpRecvThread.FOnDataRecv := DataReceive;
        FUdpRecvThread.Resume;
      except
        DoActive(False);
        raise TUDPException.Create('建立监听线程发生错误!');
      end;
    end
    else
    begin
      ErrorCode := GetLastError;
      if Assigned(FOnSocketError) then
        FOnSocketError(Self, utInit, ErrorCode);
      if ErrorCode <> 0 then
        raise TUDPException.CreateFmt('初始化套接字发生错误，错误码是%d',
          [ErrorCode]);
    end;
  end
  else // 关闭套接字
  begin
    if Assigned(FUDPRecvThread) then
    begin
      FUdpRecvThread.Stop;
      FreeAndNil(FUDPRecvThread);
    end;
    FreeSocket;
    FActive := False;
  end;
end;

procedure TLzUDPSocket.FreeSocket;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
  if FTcpSocket <> INVALID_SOCKET then
  begin
    closesocket(FTcpSocket);
    FTcpSocket := INVALID_SOCKET;
  end;
end;

function TLzUDPSocket.GetRecvBufSize: Integer;
begin
  Result := FRecvBufSize;
end;

function TLzUDPSocket.GetSendBufSize: Integer;
begin
  Result := FSendBufSize;
end;

function TLzUDPSocket.InitSocket: Boolean;
var
  saLocal: TSockAddrIn;
  bReLinten: Boolean;
  i: Integer;
begin
  result := false;
  FSocket := WSASocket(AF_INET, SOCK_DGRAM, 0, nil, 0, WSA_FLAG_OVERLAPPED);
  if FSocket = INVALID_SOCKET then
    Exit;

  //设置在TIME_WAIT状态下可以再次在相同的端口上监听
  {
  if FPort = 0 then
  begin
    Result:= True;
    Exit;
  end;
  }

  bReLinten := True;
  if setsockopt(FSocket, SOL_SOCKET, SO_REUSEADDR, @bReLinten, SizeOf(bReLinten))
    <> 0 then
    Exit;

  if setsockopt(FSocket, SOL_SOCKET, SO_BROADCAST, @FBroadcast, SizeOf(Integer))
    <> 0 then
    Exit;

  saLocal.sin_family := AF_INET;
  saLocal.sin_port := htons(FPort);
  saLocal.sin_addr.S_addr := INADDR_ANY;

  if bind(FSocket, @saLocal, SizeOf(saLocal)) = SOCKET_ERROR then
  begin
    FreeSocket;
    Exit;
  end;

  i := SizeOf(saLocal);
  GetSockName(FSocket, saLocal, i);
  FPort := ntohs(saLocal.sin_port);

  //有代理时需先建立Udp映射通道
  if FProxyInfo.Enabled then
  begin
    if not ConnectToProxy then
      Exit;
  end;

  Result := True;
end;

procedure TLzUDPSocket.DataReceive;
begin
  if Assigned(FOnDataRead) then
    FOnDataRead(Self, FPeerInfo);
end;

function TLzUDPSocket.RecvBuf(var Buf; Size: Integer; IP: longword; Port:
  Integer): integer;
var
  saRemote: TSockAddrIn;
  ret, fromlen: Integer;
  ErrorCode: Integer;
begin
  Result := 0;
  saRemote.sin_family := AF_INET;
  saRemote.sin_addr.S_addr := IP;
  saRemote.sin_port := htons(Port);
  fromlen := SizeOf(saRemote);

  if FProxyInfo.Enabled then
    ret := RecvByProxy(FSocket, Buf, Size, IP, Port)
  else
    ret := recvfrom(FSocket, Buf, Size, 0, saRemote, fromlen);

  with FPeerInfo do
  begin
    PeerIP := saRemote.sin_addr.S_addr;
    PeerPort := ntohs(saRemote.sin_port);
  end;

  if ret = SOCKET_ERROR then
  begin
    ErrorCode := GetLastError;
    if (ErrorCode <> WSAEWOULDBLOCK)
      and (ErrorCode <> WSAECONNRESET) then //不报告connection reset by peer错误
    begin
      if Assigned(FOnSocketError) then
        FOnSocketError(Self, utRecv, ErrorCode);
      if ErrorCode <> 0 then
        raise TUDPException.CreateFmt('接收数据出错。错误码是%d', [ErrorCode]);
    end;
  end
  else
    Result := ret;
end;

function TLzUDPSocket.SendBuf(var Buf; Size: Integer; IP: longword; Port:
  Integer): Boolean;
var
  ret, ErrorCode: Integer;
  saRemote: TSockAddrIn;
begin
  Result := False;

  saRemote.sin_family := AF_INET;
  saRemote.sin_port := htons(Port);
  saRemote.sin_addr.S_addr := IP;

  if saRemote.sin_addr.S_addr = INADDR_NONE then
    raise TUDPException.Create('无效的远程主机地址!');

  if FProxyInfo.Enabled then
    ret := SendByProxy(FSocket, Buf, Size, IP, Port)
  else
    ret := sendto(FSocket, Buf, Size, 0, saRemote, SizeOf(saRemote));

  if ret = SOCKET_ERROR then
  begin
    ErrorCode := GetLastError;
    if ErrorCode <> WSAEWOULDBLOCK then
    begin
      if Assigned(FOnSocketError) then
        FOnSocketError(Self, utSend, ErrorCode);
      if ErrorCode <> 0 then
        raise TUDPException.CreateFmt('发送数据时出错。错误码是%d',
          [ErrorCode]);
    end;
  end
  else
    Result := True;
end;

function TLzUDPSocket.SendText(Text:string; IP: longword; Port: integer): Boolean;
begin
  Result := SendBuf(Pointer(Text)^, Length(Text), IP, Port);
end;

procedure TLzUDPSocket.SetActive(Value: Boolean);
begin
  if FActive <> Value then
    DoActive(Value);
end;

procedure TLzUDPSocket.SetRecvBufSize(Value: Integer);
var
  ErrorCode: Integer;
begin
  if FRecvBufSize <> Value then
  begin
    ErrorCode := setsockopt(FSocket, SOL_SOCKET, SO_RCVBUF, @Value,
      sizeof(Value));
    if ErrorCode = SOCKET_ERROR then
      raise TUDPException.CreateFmt('设置接收缓冲区出错。错误码是%d',
        [GetLastError]);
    FRecvBufSize := Value;
  end;
end;

procedure TLzUDPSocket.SetSendBufSize(Value: Integer);
var
  ErrorCode: Integer;
begin
  if FSendBufSize <> Value then
  begin
    ErrorCode := setsockopt(FSocket, SOL_SOCKET, SO_SNDBUF, @Value,
      sizeof(Value));
    if ErrorCode = SOCKET_ERROR then
      raise TUDPException.CreateFmt('设置发送缓冲区错误。错误码是%d',
        [GetLastError]);
    FSendBufSize := Value;
  end;
end;

procedure TLzUDPSocket.SetTimeOut(Value: Longword);
begin
  if FTimeOut <> Value then
    FTimeOut := Value;
end;

function TLzUDPSocket.ConnectToProxy: Boolean;
var
  saProxy: TSockAddrIn;
  ret: Integer;
  bRet: Boolean;
begin
  //建立到Proxy的Tcp连接
  if FTcpSocket = INVALID_SOCKET then
    FTcpSocket := socket(AF_INET, SOCK_STREAM, 0);

  saProxy.sin_family := AF_INET;
  saProxy.sin_port := htons(FProxyInfo.Port);
  saProxy.sin_addr.S_addr := inet_addr(PChar(FProxyInfo.Address));
  ret := connect(FTcpSocket, @saProxy, SizeOf(saProxy));
  if ret = SOCKET_ERROR then
    raise Exception.CreateFmt('无法连接到代理服务器，错误码是%d',
      [WSAGetLastError]);

  {代理服务器是否需要身份验证}
  if Trim(FProxyInfo.Username) <> '' then
    bRet := Handclasp(FTcpSocket, atUserPass)
  else
    bRet := Handclasp(FTcpSocket, atNone);

  if not bRet then
  begin
    closesocket(FTcpSocket);
    raise Exception.CreateFmt('代理服务器身份验证失败!错误码是%d',
      [WSAGetLastError]);
  end;

  //建立UDP映射通道
  if not MapUdpChannel(FTcpSocket) then
  begin
    closesocket(FTcpSocket);
    raise Exception.CreateFmt('代理服务器不支持UDP!错误码是%d',
      [WSAGetLastError]);
  end;

  Result := True;
end;

function TLzUDPSocket.Handclasp(Socket: TSocket; AuthenType: TAuthenType):
  Boolean;
var
  Buf: array[0..255] of Byte;
  I, Ret: Integer;
  Username, Password: string;
begin
  Result := False;
  case AuthenType of
    // 无需验证
    atNone:
      begin
        Buf[0] := $05;
        Buf[1] := $01;
        Buf[2] := $00;
        Ret := send(Socket, Buf, 3, 0);
        if Ret = -1 then Exit;
        FillChar(Buf, 256, #0);
        Ret := recv(Socket, Buf, 256, 0);
        if Ret < 2 then Exit;
        if Buf[1] <> $00 then Exit;
        Result := True;
      end;
    // 用户名密码验证
    atUserPass:
      begin
        Buf[0] := $05; // Socks版本号
        Buf[1] := $02; // 两种认证方法
        Buf[2] := $00; // 无需校验
        Buf[3] := $02; // 需用户名密码校验
        Ret := send(Socket, Buf, 4, 0);
        if Ret = -1 then Exit;
        FillChar(Buf, 256, #0);
        Ret := recv(Socket, Buf, 256, 0);
        if Ret < 2 then Exit;
        if Buf[1] <> $02 then Exit;
        Username := FProxyInfo.Username;
        Password := FProxyInfo.Password;
        FillChar(Buf, 256, #0);
        Buf[0] := $01;
        Buf[1] := Length(Username);
        for I := 0 to Buf[1] - 1 do
          Buf[2 + I] := Ord(Username[I + 1]);
        Buf[2 + Length(Username)] := Length(Password);
        for I := 0 to Buf[2 + Length(Username)] - 1 do
          Buf[3 + Length(Username) + I] := Ord(Password[I + 1]);
        Ret := send(Socket, Buf, Length(Username) + Length(Password) + 3, 0);
        if Ret = -1 then Exit;
        Ret := recv(Socket, Buf, 256, 0);
        if Ret = -1 then Exit;
        if Buf[1] <> $00 then Exit;
        Result := True;
      end;
  end;
end;

function TLzUDPSocket.MapUdpChannel(Socket: TSocket): Boolean;
var
  saLocal: TSockAddrIn;
  NameLen: Integer;
  ProxyAddr: TInAddr;
  ProxyPort: Word;
  Buf: array[0..255] of Byte;
begin
  Result := False;
  NameLen := SizeOf(saLocal);
  getsockname(FSocket, saLocal, NameLen);
  Buf[0] := $05; //协议版本Socks5
  Buf[1] := $03; //Socks命令:UDP
  Buf[2] := $00; //保留
  Buf[3] := $01; //地址类型IPv4
  CopyMemory(@Buf[4], @saLocal.sin_addr, 4);
  CopyMemory(@Buf[8], @saLocal.sin_port, 2);
  send(Socket, Buf, 10, 0);
  FillChar(Buf, 256, #0);
  recv(Socket, Buf, 256, 0);
  if (Buf[0] <> $05) and (Buf[1] <> $00) then
    Exit;
  CopyMemory(@ProxyAddr, @Buf[4], 4); //获取Proxy的映射地址
  CopyMemory(@ProxyPort, @Buf[8], 2); //获取Proxy的映射端口号

  FUdpProxyAddr.sin_family := AF_INET;
  FUdpProxyAddr.sin_port := ProxyPort;
  FUdpProxyAddr.sin_addr := ProxyAddr;

  Result := True;
end;

function TLzUDPSocket.SendByProxy(Socket: TSocket; var buf; len: Integer;
  RemoteIP: longword; RemotePort: Integer): Integer;
var
  TempBuf: array[0..1023] of Byte;
  saRemote: TSockAddrIn;
begin
  saRemote.sin_family := AF_INET;
  saRemote.sin_port := htons(RemotePort);
  saRemote.sin_addr.S_addr := RemoteIP;
  // 加上报头
  FillChar(TempBuf, 1023, $0);
  TempBuf[0] := $00; //保留
  TempBuf[1] := $00; //保留
  TempBuf[2] := $00; //是否分段重组(此处不用)
  TempBuf[3] := $01; //IPv4
  CopyMemory(@TempBuf[4], @saRemote.sin_addr, 4); //代理服务器地址
  CopyMemory(@TempBuf[8], @saRemote.sin_port, 2); //代理服务器端口
  CopyMemory(@TempBuf[10], @buf, len); //实际数据
  Result := sendto(Socket, TempBuf, len + 10, 0, FUdpProxyAddr,
    SizeOf(FUdpProxyAddr));
  if Result = SOCKET_ERROR then
    raise Exception.CreateFmt('发送数据错误!错误号是%d', [WSAGetLastError]);
end;

function TLzUDPSocket.RecvByProxy(Socket: TSocket; var buf; len: Integer;
  RemoteIP: longword; RemotePort: Integer): Integer;
var
  TempBuf: array[0..1023] of Byte;
  saRemote: TSockAddrIn;
  fromlen: Integer;
begin
  FillChar(TempBuf, 1024, #0);
  saRemote.sin_family := AF_INET;
  saRemote.sin_port := htons(RemotePort);
  saRemote.sin_addr.S_addr := RemoteIP;
  fromlen := SizeOf(saRemote);
  Result := recvfrom(Socket, TempBuf, len, 0, saRemote, fromlen);
  if Result = SOCKET_ERROR then
    raise Exception.CreateFmt('接收数据错误!错误号是%d', [WSAGetLastError]);
  Assert(TempBuf[0] = $00); //保留
  Assert(TempBuf[1] = $00); //保留
  Assert(TempBuf[2] = $00); //是否分段重组
  Assert(TempBuf[3] = $01); //IPv4
  CopyMemory(@saRemote.sin_addr, @TempBuf[4], 4); //代理服务器地址
  CopyMemory(@saRemote.sin_port, @TempBuf[8], 2); //代理服务器端口
  CopyMemory(@buf, @TempBuf[10], len); //实际数据
end;

{ TLzUDPRecvThread }

constructor TLzUDPRecvThread.Create(CreateSuspended: Boolean; AUdpSocket:
  TLzUDPSocket);
begin
  inherited Create(CreateSuspended);
  FSocket := AUDPSocket;
  FEvent := WSA_INVALID_EVENT;
  InitEvent;
end;

destructor TLzUDPRecvThread.Destroy;
begin
  if not Terminated then
    Stop;
  FreeEvent;
  inherited Destroy;
end;

procedure TLzUDPRecvThread.Execute;
var
  ErrorCode: Integer;
begin
  while not Terminated do
  begin
    ErrorCode := WSAWaitForMultipleEvents(
      1, //事件数量
      @FEvent, //事件处理函数
      False, //有一个事件触发时就返回
      FSocket.FTimeOut, // 超时时间
      False //函数返回时，不执行I/O例程
      );

    if Terminated then
      Break;

    if ErrorCode = WAIT_IO_COMPLETION then
    begin
      Break;
    end
    else
    begin
      WSAResetEvent(FEvent);
      if ErrorCode = WSA_WAIT_TIMEOUT then
      begin
        if Assigned(FSocket.FOnTimeOut) then
          FSocket.FOnTimeOut;
//          Synchronize(FSocket.FOnTimeOut);
      end
      else
        if Assigned(FOnDataRecv) then
          FOnDataRecv;
//          Synchronize(FOnDataRecv);
    end;
  end;
end;

procedure TLzUDPRecvThread.FreeEvent;
begin
  if FEvent <> WSA_INVALID_EVENT then
    WSACloseEvent(FEvent);
end;

procedure TLzUDPRecvThread.InitEvent;
var
  ErrorCode: Integer;
begin
  FEvent := WSACreateEvent;
  if FEvent = WSA_INVALID_EVENT then
    raise TUDPException.CreateFmt('创建套接字事件句柄出错。错误码是%d',
      [WSAGetLastError]);

  ErrorCode := WSAEventSelect(FSocket.FSocket, FEvent, FD_READ);
  if ErrorCode = SOCKET_ERROR then
    raise TUDPException.CreateFmt('设置套接字事件句柄出错。错误码是%d',
      [WSAGetLastError]);
end;

procedure TLzUDPRecvThread.Stop;
begin
  Terminate;
  SetEvent(FEvent);
  WaitFor;
end;

end.

