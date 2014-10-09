unit RawTcpClient;

interface

uses
  Classes, iocpRawSocket, SysUtils, iocpWinsock2;

{$if CompilerVersion < 23}
type
     NativeUInt = Cardinal;
     IntPtr = Cardinal;
{$ifend}

type

  TRawTcpClient = class(TComponent)
  private
    FHost: String;
    FPort: Integer;
    FRawSocket: TRawSocket;
    FActive:Boolean;
    FReadTimeOut: Integer;
    procedure SetActive(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;     

    procedure Connect;
    procedure Disconnect;

    function RecvBuffer(buf: Pointer; len: cardinal): Integer;
    function sendBuffer(buf: Pointer; len: cardinal): Integer;

    /// <summary>
    ///  recv buffer
    /// </summary>
    procedure recv(buf: Pointer; len: cardinal);

    property Active: Boolean read FActive write SetActive;
  published
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;


    /// <summary>
    ///   unit ms
    /// </summary>
    property ReadTimeOut: Integer read FReadTimeOut write FReadTimeOut;


  end;

implementation

var
  __WSAStarted:boolean;

procedure WSAStart;
var
  lvRET: Integer;
  WSData: TWSAData;
begin
  lvRET := WSAStartup($0202, WSData);
  if lvRET <> 0 then RaiseLastOSError;
end;


constructor TRawTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRawSocket := TRawSocket.Create();
  FReadTimeOut := 30000;
end;

destructor TRawTcpClient.Destroy;
begin
  FRawSocket.Free;
  inherited Destroy;
end;

procedure TRawTcpClient.connect;
begin
  if FActive then exit;

  if not __WSAStarted then
  begin
    WSAStart;
    __WSAStarted := true;
  end;
  
    
  FRawSocket.createTcpSocket;
  FRawSocket.setReadTimeOut(FReadTimeOut);
  FActive := FRawSocket.connect(FHost, FPort);
  if not FActive then
  begin
    RaiseLastOSError;
  end;
end;

procedure TRawTcpClient.Disconnect;
begin
  if not FActive then Exit;

  FRawSocket.close;

  FActive := false;
end;

procedure TRawTcpClient.recv(buf: Pointer; len: cardinal);
var
  lvTempL :Integer;
  lvReadL :Cardinal;
  lvPBuf:Pointer;
begin
  lvReadL := 0;
  lvPBuf := buf;
  while lvReadL < len do
  begin
    lvTempL := FRawSocket.Recv(lvPBuf^, len - lvReadL);
    if lvTempL = -1 then
    begin
      RaiseLastOSError;
    end else
    begin
      lvPBuf := Pointer(IntPtr(lvPBuf) + Cardinal(lvTempL));
      lvReadL := lvReadL + Cardinal(lvTempL);
    end;
  end;
end;

function TRawTcpClient.RecvBuffer(buf: Pointer; len: cardinal): Integer;
begin
  Result := FRawSocket.Recv(buf^, len);
  if Result = SOCKET_ERROR then
  begin
    RaiseLastOSError;
  end;
end;

function TRawTcpClient.SendBuffer(buf: Pointer; len: cardinal): Integer;
begin
  Result := FRawSocket.Send(buf^, len);
  if Result = SOCKET_ERROR then
  begin
    RaiseLastOSError;
  end;
end;

procedure TRawTcpClient.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
    begin
      Connect;
    end else
    begin
      Disconnect;
    end;
  end;
end;

end.
