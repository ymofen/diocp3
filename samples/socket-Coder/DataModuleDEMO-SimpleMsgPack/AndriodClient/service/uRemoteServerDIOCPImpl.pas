unit uRemoteServerDIOCPImpl;

interface

uses
  uIRemoteServer,
  DTcpClient,
  uDTcpClientCoderImpl,
  uStreamCoderSocket,
  SimpleMsgPack,
  Classes,
  SysUtils,
{$IFDEF POSIX}
    DZipTools,
{$ELSE}
    uZipTools,
{$ENDIF}
  uICoderSocket;

type
  TRemoteServerDIOCPImpl = class(TInterfacedObject, IRemoteServer)
  private
    FTcpClient: TDTcpClient;
    FCoderSocket: ICoderSocket;
    FMsgPack:TSimpleMsgPack;
    FSendStream:TMemoryStream;
    FRecvStream:TMemoryStream;
  protected
    /// <summary>
    ///   执行远程动作
    /// </summary>
    function Execute(pvCmdIndex: Integer; var vData: OleVariant): Boolean; stdcall;
  public
    constructor Create;
    procedure setHost(pvHost: string);
    procedure setPort(pvPort:Integer);
    procedure open;
    destructor Destroy; override;
  end;

implementation

constructor TRemoteServerDIOCPImpl.Create;
begin
  inherited Create;
  FTcpClient := TDTcpClient.Create(nil);
  FCoderSocket := TDTcpClientCoderImpl.Create(FTcpClient);
  
  FMsgPack := TSimpleMsgPack.Create;
  FRecvStream := TMemoryStream.Create;
  FSendStream := TMemoryStream.Create;
end;

destructor TRemoteServerDIOCPImpl.Destroy;
begin
  FCoderSocket := nil;
  FTcpClient.Disconnect;
  FTcpClient.Free;
  FMsgPack.Free;
  FRecvStream.Free;
  FSendStream.Free;
  inherited Destroy;
end;

{ TRemoteServerDIOCPImpl }

function TRemoteServerDIOCPImpl.Execute(pvCmdIndex: Integer; var vData:
    OleVariant): Boolean;
begin
  if not FTcpClient.Active then FTcpClient.Connect;
  FSendStream.Clear;
  FRecvStream.Clear;
  FMsgPack.Clear;
  FMsgPack.ForcePathObject('cmd.index').AsInteger := pvCmdIndex;
  FMsgPack.ForcePathObject('cmd.data').AsVariant := vData;
  FMsgPack.EncodeToStream(FSendStream);

{$IFDEF POSIX}
  TDZipTools.ZipStream(FSendStream, FSendStream);
{$ELSE}
  TZipTools.compressStreamEX(FSendStream);
{$ENDIF}


  TStreamCoderSocket.SendObject(FCoderSocket, FSendStream);

  TStreamCoderSocket.RecvObject(FCoderSocket, FRecvStream);

{$IFDEF POSIX}
  TDZipTools.UnZipStream(FRecvStream, FRecvStream);
{$ELSE}
  TZipTools.unCompressStreamEX(FRecvStream);
{$ENDIF}
  FRecvStream.Position := 0;
  
  FMsgPack.DecodeFromStream(FRecvStream);

  Result := FMsgPack.ForcePathObject('__result.result').AsBoolean;

  if not Result then
    if FMsgPack.ForcePathObject('__result.msg').AsString <> '' then
    begin
      raise Exception.Create(FMsgPack.ForcePathObject('__result.msg').AsString);
    end;

  vData := FMsgPack.ForcePathObject('__result.data').AsVariant;
end;

procedure TRemoteServerDIOCPImpl.open;
begin
  FTcpClient.Disconnect;
  FTcpClient.Connect;
end;

procedure TRemoteServerDIOCPImpl.setHost(pvHost: string);
begin
  FTcpClient.Host := pvHost;
end;

procedure TRemoteServerDIOCPImpl.setPort(pvPort: Integer);
begin
  FTcpClient.Port := pvPort;  
end;

end.
