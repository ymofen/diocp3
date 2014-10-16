unit uMyClientContext;

interface

uses
  uIOCPCentre, udmMain, Classes,  SysUtils, uZipTools, SimpleMsgPack;

type
  TMyClientContext = class(TIOCPCoderClientContext)
  private
    FdmMain:TdmMain;

  protected

    procedure OnDisconnected; override;

    procedure OnConnected;override;

  protected

    /// <summary>
    ///   on received a object
    /// </summary>
    /// <param name="pvDataObject"> (TObject) </param>
    procedure dataReceived(const pvDataObject:TObject); override;
  public

    
  end;

implementation

{ TMyClientContext }

procedure TMyClientContext.dataReceived(const pvDataObject: TObject);
var
  lvMsgPack, lvMsgPack2:TSimpleMsgPack;
  lvStream :TStream;
  lvStream2:TMemoryStream;
  vData:OleVariant;
  lvResult:Boolean;
  vMsg:String;
begin
  lvMsgPack := TSimpleMsgPack.Create;
  try
    try
      if FdmMain = nil then FdmMain := TdmMain.Create(nil);

      lvStream := TStream(pvDataObject);
      lvStream.Position := 0;

      // upZip
      TZipTools.unCompressStreamEX(lvStream);

      lvStream.Position := 0;
      
      // unpack
      lvMsgPack.DecodeFromStream(lvStream);

      // get param
      vData := lvMsgPack.ForcePathObject('cmd.data').AsVariant;

      // invoke dataModule function
      lvResult := FdmMain.Execute(lvMsgPack.ForcePathObject('cmd.index').AsInteger,
        vData, vMsg);

      // write result info
      lvMsgPack.Clear;
      lvMsgPack.ForcePathObject('__result.result').AsBoolean := lvResult;
      lvMsgPack.ForcePathObject('__result.data').AsVariant := vData;
      lvMsgPack.ForcePathObject('__result.msg').AsString := vMsg;
    except
      on E:Exception do
      begin
        lvMsgPack.Clear;
        lvMsgPack.ForcePathObject('__result.result').AsBoolean := false;
        lvMsgPack.ForcePathObject('__result.msg').AsString := e.Message;
      end;
    end;

    lvStream.Size := 0;
    lvMsgPack.EncodeToStream(lvStream);

    lvStream.Position := 0;

    // zipStream
    TZipTools.compressStreamEX(lvStream);
    lvStream.Position := 0;

    // send to client
    self.writeObject(lvStream);
  finally
    lvMsgPack.Free;
  end;

end;

procedure TMyClientContext.OnConnected;
begin
  inherited;
end;

procedure TMyClientContext.OnDisconnected;
begin
  inherited;
  if FdmMain <> nil then
  begin
    FdmMain.Free;
    FdmMain := nil;
  end;
end;

end.
