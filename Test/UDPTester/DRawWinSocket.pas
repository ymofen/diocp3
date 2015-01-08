(*
   unit owner: d10.天地弦
   a cross platform unit

   2014-11-11 12:59:08
   + add GetIpAddrByName
   thanks for @广州-cyw
*)
unit DRawWinSocket;

interface

uses
  SysUtils
  , Windows, winsock
  , DWinSocket2;

{$if CompilerVersion < 23}
type
     NativeUInt = Cardinal;
     IntPtr = Cardinal;
{$ifend}

const
  SOCKET_ERROR   = -1;
  {$EXTERNALSYM SOCKET_ERROR}

{$IFDEF MSWINDOWS}
  SD_RECEIVE = $00;
  {$EXTERNALSYM SD_RECEIVE}
  SD_SEND    = $01;
  {$EXTERNALSYM SD_SEND}
  SD_BOTH    = $02;
  {$EXTERNALSYM SD_BOTH}
{$ENDIF}

type
  TDSocketState = (ssDisconnected, ssConnected, ssConnecting, ssListening, ssAccepting);

  TDRawWinSocket = class(TObject)
  private
    FSockaddr: winsock.sockaddr_in;
    FSocketHandle:THandle;
  public
    function bind(const pvAddr: string; pvPort: Integer): Boolean;
    procedure CreateTcpSocket;
    procedure CreateUdpSocket;
    procedure CreateOverlappedUdpSocket();

    function RecvBuf(var data; const len: Cardinal): Integer;
    function PeekBuf(var data; const len: Cardinal): Integer;
    function SendBuf(const data; const len: Cardinal): Integer;
    function SendBufTo(const data; const len: Integer): Integer;
    function Connect(const pvAddr: string; pvPort: Integer): Boolean;
    function RecvdCount: Integer;


    procedure SetConnectInfo(const pvAddr: string; pvPort: Integer);

    /// <summary>
    ///   can send?
    ///  unit 's
    /// </summary>
    function Writeable(pvTimeOut:Integer): Integer;

    /// <summary>
    ///   check can recv
    ///    unit usec
    /// </summary>
    function Readable(pvTimeOut:Integer): Boolean;

    /// <summary>
    ///   Peer Info
    ///    no test
    /// </summary>
    function GetPeerInfo(var vIp: longword; var vPort: Integer): Integer;

    /// <summary>
    ///   set NonBlock mode
    /// </summary>
    function SetNonBlock(pvBlock:Boolean): Integer;

    /// <summary>
    ///  resove host
    /// </summary>
    function GetIpAddrByName(const pvHost: string): string;

    /// <summary>
    ///   set recv time out
    ///    unit is ms
    /// </summary>
    function SetReadTimeOut(const pvTimeOut: Cardinal): Integer;


    /// <summary>
    ///   set send time out
    ///    unit is ms
    /// </summary>
    function SetSendTimeOut(const pvTimeOut: Cardinal): Integer;

    procedure Close;

    function IsValidSocketHandle: Boolean;
  public
    property SocketHandle: THandle read FSocketHandle;
  end;


implementation


{$IFDEF MSWINDOWS}
const
  winsocket = 'wsock32.dll';

var
  __WSAStartupDone:Boolean;

{$EXTERNALSYM send}
function send(s: TSocket; const Buf; len, flags: Integer): Integer; stdcall;
  external winsocket name 'send';
{$EXTERNALSYM sendto}
function sendto(s: TSocket; const Buf; len, flags: Integer; var addrto: winsock.TSockAddr;
  tolen: Integer): Integer; stdcall; external    winsocket name 'sendto';

/// <summary>
///  compare target, cmp_val same set target = new_val
///    return old value
/// </summary>
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean): Boolean;
asm
  lock cmpxchg [ecx], dl
end;

procedure __CheckWSAStartup;
var
  AData: WSAData;
begin
  if lock_cmp_exchange(False, True, __WSAStartupDone) = False then
  begin
    if WSAStartup(MakeWord(1, 1), AData) <> 0 then
    begin
      __WSAStartupDone := false;
      RaiseLastOSError(WSAGetLastError);
    end;
  end;
end;

{$ENDIF}

{$IFDEF POSIX}
function TranslateTInAddrToString(var AInAddr): string;
type
  TIdSunB = packed record
    s_b1, s_b2, s_b3, s_b4: Byte;
  end;

  TIdSunW = packed record
    s_w1, s_w2: Word;
  end;
  PIdIn4Addr = ^TIdIn4Addr;
  TIdIn4Addr = packed record
    case integer of
        0: (S_un_b: TIdSunB);
        1: (S_un_w: TIdSunW);
        2: (S_addr: LongWord);
  end;
begin
  Result := IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b1) + '.'   {Do not Localize}
            + IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b2) + '.' {Do not Localize}
            + IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b3) + '.' {Do not Localize}
            + IntToStr(TIdIn4Addr(AInAddr).S_un_b.s_b4);
end;


{$ENDIF}

{ TDRawWinSocket }

function TDRawWinSocket.bind(const pvAddr: string; pvPort: Integer): Boolean;
var
  s :String;
begin
  FillChar(FSockaddr, SizeOf(sockaddr_in), 0);
  FSockaddr.sin_family := AF_INET;
  FSockaddr.sin_port := htons(pvPort);
  s := pvAddr;
  if s = '' then
  begin
    s := '0.0.0.0';
  end;
  FSockaddr.sin_addr.s_addr :=inet_addr(PAnsichar(UTF8Encode(s)));
  Result := winsock.bind(FSocketHandle, FSockaddr, sizeof(sockaddr_in))  = 0;
end;

procedure TDRawWinSocket.Close;
var
  lvTempSocket: THandle;
begin
  lvTempSocket := FSocketHandle;
  ///INVALID_SOCKET
  if lvTempSocket <> INVALID_HANDLE_VALUE then
  begin
    FSocketHandle := INVALID_HANDLE_VALUE;
    {$IFDEF MSWINDOWS}
      shutdown(lvTempSocket, SD_BOTH);
      closesocket(lvTempSocket);
    {$ELSE}
      __close(lvTempSocket);
    {$ENDIF}
  end;
end;

function TDRawWinSocket.Connect(const pvAddr: string; pvPort: Integer): Boolean;
{$IFDEF POSIX}
{$ELSE}
{$ENDIF}
begin
  FillChar(FSockaddr, SizeOf(sockaddr_in), 0);
  FSockaddr.sin_family := AF_INET;
  FSockaddr.sin_port := htons(pvPort);
{$IFDEF POSIX}
  FSockaddr.sin_addr.s_addr :=inet_addr(MarshaledAString(UTF8Encode(pvAddr)));
  Result := Posix.SysSocket.Connect(FSocketHandle, sockaddr(FSockaddr), sizeof(sockaddr_in))  = 0;
{$ELSE}
  FSockaddr.sin_addr.s_addr :=inet_addr(PAnsichar(AnsiString(pvAddr)));
  Result := winsock.Connect(FSocketHandle, FSockaddr, sizeof(sockaddr_in))  = 0;
{$ENDIF}
end;

procedure TDRawWinSocket.CreateOverlappedUdpSocket;
begin
  __CheckWSAStartup;
  FSocketHandle := WSASocket(AF_INET, SOCK_DGRAM, IPPROTO_UDP, nil, 0, WSA_FLAG_OVERLAPPED);
  if FSocketHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
end;

procedure TDRawWinSocket.CreateTcpSocket;
begin
{$IFDEF MSWINDOWS}
  __CheckWSAStartup;
{$ENDIF}
  FSocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocketHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
end;

procedure TDRawWinSocket.CreateUdpSocket;
begin
{$IFDEF MSWINDOWS}
  __CheckWSAStartup;
{$ENDIF}
  FSocketHandle := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
end;

function TDRawWinSocket.Readable(pvTimeOut:Integer): Boolean;
var
  lvFDSet:winsock.TFDSet;
  lvTime_val: TTimeval;

begin
  winsock.FD_ZERO(lvFDSet);
  winsock.FD_SET(FSocketHandle, lvFDSet);

  lvTime_val.tv_sec := pvTimeOut div 1000;
  lvTime_val.tv_usec :=  1000 * (pvTimeOut mod 1000);
  Result := winsock.select(0, @lvFDSet, nil, nil, @lvTime_val) > 0;
end;

function TDRawWinSocket.RecvBuf(var data; const len: Cardinal): Integer;
begin
  Result := winsock.recv(FSocketHandle, data, len, 0);
end;

function TDRawWinSocket.SendBuf(const data; const len: Cardinal): Integer;
begin
  Result := Send(FSocketHandle, data, len, 0);
end;

function TDRawWinSocket.SendBufTo(const data; const len: Integer): Integer;
begin
  Result := sendto(FSocketHandle, data, len, 0, FSockaddr, sizeof(winsock.sockaddr_in));

end;

function TDRawWinSocket.SetReadTimeOut(const pvTimeOut: Cardinal): Integer;
begin
{$IFDEF POSIX}
  // not test
  Result := setsockopt(FSocketHandle,
   SOL_SOCKET, SO_RCVTIMEO, pvTimeOut, SizeOf(Cardinal));
{$ELSE} 
  Result := setsockopt(FSocketHandle,
   SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@pvTimeOut), SizeOf(Cardinal));
{$ENDIF}
end;


function TDRawWinSocket.SetSendTimeOut(const pvTimeOut: Cardinal): Integer;
begin
{$IFDEF POSIX}
  // not test
  Result := setsockopt(FSocketHandle,
   SOL_SOCKET, SO_SNDTIMEO, pvTimeOut, SizeOf(Cardinal));
{$ELSE}
  Result := setsockopt(FSocketHandle,
   SOL_SOCKET, SO_SNDTIMEO, PAnsiChar(@pvTimeOut), SizeOf(Cardinal));
{$ENDIF}
end;

function TDRawWinSocket.Writeable(pvTimeOut:Integer): Integer;
var
  lvFDSet:winsock.TFDSet;
  lvTime_val: winsock.TTimeval;

begin

  winsock.FD_ZERO(lvFDSet);
  winsock.FD_SET(FSocketHandle, lvFDSet);

  lvTime_val.tv_sec := pvTimeOut;
  lvTime_val.tv_usec := 0;
  Result := winsock.select(0, nil, @lvFDSet, nil, @lvTime_val);

end;

function TDRawWinSocket.GetIpAddrByName(const pvHost: string): string;
var
  lvhostInfo: PHostEnt;
begin
  lvhostInfo := gethostbyname(PAnsiChar(AnsiString(pvHost)));
  if lvhostInfo = nil then
    RaiseLastOSError;

  Result := inet_ntoa(PInAddr(lvhostInfo^.h_addr_list^)^);
end;

function TDRawWinSocket.GetPeerInfo(var vIp: longword; var vPort: Integer):
    Integer;
{$IFDEF POSIX}
{$ELSE}
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
{$ENDIF}
begin
{$IFDEF POSIX}
{$ELSE}
  
  Size := SizeOf(SockAddrIn);
  result := getpeername(FSocketHandle, TSockAddr(SockAddrIn), Size);
  vIp := SockAddrIn.sin_addr.S_addr;
  vPort := ntohs(SockAddrIn.sin_port);
{$ENDIF}
end;

function TDRawWinSocket.IsValidSocketHandle: Boolean;
begin
   Result := FSocketHandle <> INVALID_HANDLE_VALUE;
end;

function TDRawWinSocket.PeekBuf(var data; const len:Cardinal): Integer;
begin
  Result := winsock.recv(FSocketHandle, data, len, MSG_PEEK);
end;

function TDRawWinSocket.RecvdCount: Integer;
var
  Temp : winsock.u_long;
begin
  if winsock.ioctlsocket(FSocketHandle, FIONREAD, Temp) = SOCKET_ERROR then
  begin
    Result := -1;
  end else
  begin
    Result := Temp;
  end;
end;

procedure TDRawWinSocket.SetConnectInfo(const pvAddr: string; pvPort: Integer);
begin
  FillChar(FSockaddr, SizeOf(sockaddr_in), 0);
  FSockaddr.sin_family := AF_INET;
  FSockaddr.sin_port := htons(pvPort);
  FSockaddr.sin_addr.s_addr :=winsock.inet_addr(PAnsichar(AnsiString(pvAddr)));

end;

function TDRawWinSocket.SetNonBlock(pvBlock:Boolean): Integer;
var
  lvFlag : Integer;
begin
  if pvBlock then lvFlag := 0 else lvFlag := 1;
  Result := winsock.ioctlsocket(SocketHandle, FIONBIO, lvFlag);
end;






initialization
{$IFDEF MSWINDOWS}

{$ENDIF}

end.
