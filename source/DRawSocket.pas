(*
   unit owner: d10.ÃÏµÿœ“
   a cross platform unit
*)
unit DRawSocket;

interface

uses
  SysUtils
{$IFDEF POSIX}
    , Posix.Base, Posix.SysSocket, Posix.arpainet, Posix.NetinetIn, Posix.UniStd
{$ELSE}
    , Windows, winsock
{$ENDIF};

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
  TDRawSocket = class(TObject)
  private
    FSockaddr: sockaddr_in;
    FSocketHandle:THandle;
  public
    procedure CreateTcpSocket;
    procedure CreateUdpSocket;
    function RecvBuf(var data; const len: Cardinal): Integer;
    function SendBuf(const data; const len: Cardinal): Integer;
    function SendBufTo(const data; const len: Integer): Integer;
    function Connect(const pvAddr: string; pvPort: Integer): Boolean;
    procedure Close;
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
function sendto(s: TSocket; const Buf; len, flags: Integer; var addrto: TSockAddr;
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

procedure TDRawSocket.Close;
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

function TDRawSocket.Connect(const pvAddr: string; pvPort: Integer): Boolean;
begin
  FillChar(FSockaddr, SizeOf(sockaddr_in), 0);
  FSockaddr.sin_family := AF_INET;
  FSockaddr.sin_port := htons(pvPort);
  FSockaddr.sin_addr.s_addr :=inet_addr(PAnsichar(UTF8Encode(pvAddr)));
{$IFDEF POSIX}
  Result := Posix.SysSocket.Connect(FSocketHandle, sockaddr(FSockaddr), sizeof(sockaddr_in))  = 0;
{$ELSE}
  Result := winsock.Connect(FSocketHandle, FSockaddr, sizeof(sockaddr_in))  = 0;
{$ENDIF}
end;

procedure TDRawSocket.CreateTcpSocket;
begin
{$IFDEF MSWINDOWS}
  __CheckWSAStartup;
{$ENDIF}
  FSocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocketHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
end;

procedure TDRawSocket.CreateUdpSocket;
begin
{$IFDEF MSWINDOWS}
  __CheckWSAStartup;
{$ENDIF}
  FSocketHandle := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
end;

function TDRawSocket.RecvBuf(var data; const len: Cardinal): Integer;
begin
  Result := recv(FSocketHandle, data, len, 0);
end;

function TDRawSocket.SendBuf(const data; const len: Cardinal): Integer;
begin
  Result := Send(FSocketHandle, data, len, 0);
end;

function TDRawSocket.SendBufTo(const data; const len: Integer): Integer;
begin
{$IFDEF POSIX}
  Result := sendto(FSocketHandle, data, len, 0, sockaddr(FSockaddr), sizeof(sockaddr_in));
{$ELSE}
  Result := sendto(FSocketHandle, data, len, 0, FSockaddr, sizeof(sockaddr_in));
{$ENDIF}
end;




initialization
{$IFDEF MSWINDOWS}

{$ENDIF}

end.
