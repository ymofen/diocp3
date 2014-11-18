(*
   unit owner: d10.天地弦
   a cross platform unit

   2014-11-18 10:36:01
     fix Recv bug in Posix( return zero when fail)
     thanks for 广州-cyw   
*)
unit DTcpClient;

interface

uses
  SysUtils, Classes, DRawSocket; 


type
  TDTcpClient = class(TComponent)
  private
    FActive: Boolean;
    FHost: String;
    FPort: Integer;
    FRawSocket: TDRawSocket;
    FReadTimeOut: Integer;
    procedure SetActive(const Value: Boolean);
    
    procedure CheckSocketResult(pvSocketResult:Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    /// <summary>
    ///  recv buffer
    /// </summary>
    procedure recv(buf: Pointer; len: cardinal);
    function RecvBuffer(buf: Pointer; len: cardinal): Integer;
    function sendBuffer(buf: Pointer; len: cardinal): Integer;
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

constructor TDTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRawSocket := TDRawSocket.Create;
  FReadTimeOut := 30000;
end;

destructor TDTcpClient.Destroy;
begin
  FRawSocket.Free;
  inherited Destroy;
end;

procedure TDTcpClient.CheckSocketResult(pvSocketResult: Integer);
begin
  ///  Posix, fail return 0
  ///  ms_windows, fail return -1
  if (pvSocketResult = -1) or (pvSocketResult = 0) then
  begin
    RaiseLastOSError;
  end;
end;

procedure TDTcpClient.Connect;
var
  lvIpAddr:String;
begin
  if FActive then exit;

  FRawSocket.createTcpSocket;
  //FRawSocket.setReadTimeOut(FReadTimeOut);
  //lvIpAddr := FHost;

  // may domain name
  lvIpAddr := FRawSocket.GetIpAddrByName(FHost);

  FActive := FRawSocket.connect(lvIpAddr, FPort);
  if not FActive then
  begin
    RaiseLastOSError;
  end;
end;

procedure TDTcpClient.Disconnect;
begin
  if not FActive then Exit;

  FRawSocket.close;

  FActive := false;
end;

procedure TDTcpClient.recv(buf: Pointer; len: cardinal);
var
  lvTempL :Integer;
  lvReadL :Cardinal;
  lvPBuf:Pointer;
begin
  lvReadL := 0;
  lvPBuf := buf;
  while lvReadL < len do
  begin
    lvTempL := FRawSocket.RecvBuf(lvPBuf^, len - lvReadL);
    
    CheckSocketResult(lvTempL);
    
    lvPBuf := Pointer(IntPtr(lvPBuf) + Cardinal(lvTempL));
    lvReadL := lvReadL + Cardinal(lvTempL);
  end;
end;

function TDTcpClient.RecvBuffer(buf: Pointer; len: cardinal): Integer;
begin
  Result := FRawSocket.RecvBuf(buf^, len);
  CheckSocketResult(Result);
end;

function TDTcpClient.sendBuffer(buf: Pointer; len: cardinal): Integer;
begin
  Result := FRawSocket.SendBuf(buf^, len);
  CheckSocketResult(Result);
end;

procedure TDTcpClient.SetActive(const Value: Boolean);
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
