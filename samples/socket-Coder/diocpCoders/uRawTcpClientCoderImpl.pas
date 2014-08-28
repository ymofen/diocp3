unit uRawTcpClientCoderImpl;

interface

uses
  uICoderSocket, RawTcpClient;

type
  TRawTcpClientCoderImpl = class(TInterfacedObject, ICoderSocket)
  private
    FTcpClient: TRawTcpClient;
  protected
    function sendBuf(buf:Pointer; len:Cardinal):Cardinal; stdcall;
    function recvBuf(buf:Pointer; len:Cardinal):Cardinal; stdcall;    
    procedure closeSocket; stdcall;
  public
    constructor Create(ATcpClient: TRawTcpClient);
    destructor Destroy; override;
  end;

implementation

constructor TRawTcpClientCoderImpl.Create(ATcpClient: TRawTcpClient);
begin
  inherited Create;
  FTcpClient := ATcpClient;
end;

destructor TRawTcpClientCoderImpl.Destroy;
begin
  inherited Destroy;
end;

{ TRawTcpClientCoderImpl }

procedure TRawTcpClientCoderImpl.closeSocket;
begin
  FTcpClient.Disconnect;
end;

function TRawTcpClientCoderImpl.recvBuf(buf: Pointer; len: Cardinal): Cardinal;
begin
  Result := FTcpClient.RecvBuffer(buf, len);
end;

function TRawTcpClientCoderImpl.sendBuf(buf: Pointer; len: Cardinal): Cardinal;
begin
  Result := FTcpClient.sendBuffer(buf, len);
end;

end.
