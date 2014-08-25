unit uMyClientContext;

interface

uses
  uIOCPCentre, udmMain, Classes, qmsgpack, SysUtils, uZipTools;

type
  TMyClientContext = class(TIOCPCoderClientContext)
  private
    FdmMain:TdmMain;

  protected

    procedure OnDiscounnected;override;

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
  lvMsgPack:TQMsgPack;
  lvStream:TStream;
  vData:OleVariant;
  lvResult:Boolean;
begin
  lvMsgPack := TQMsgPack.Create;
  try
    try
      if FdmMain = nil then FdmMain := TdmMain.Create(nil);

      lvStream :=TStream(pvDataObject);
      lvStream.Position := 0;

      // upZip
      TZipTools.unCompressStreamEX(lvStream);

      lvStream.Position := 0;
      
      // unpack
      lvMsgPack.LoadFromStream(lvStream);

      // get param
      vData := lvMsgPack.ForcePath('cmd.data').AsVariant;

      // invoke dataModule function
      lvResult := FdmMain.Execute(lvMsgPack.ForcePath('cmd.index').AsInteger, vData);

      // write result info
      lvMsgPack.Clear;
      lvMsgPack.ForcePath('__result.result').AsBoolean := lvResult;
      lvMsgPack.ForcePath('__result.data').AsVariant := vData;
    except
      on E:Exception do
      begin
        lvMsgPack.Clear;
        lvMsgPack.ForcePath('__result.result').AsBoolean := false;
        lvMsgPack.ForcePath('__result.msg').AsString := e.Message;
      end;
    end;

    lvStream.Size := 0;
    lvMsgPack.SaveToStream(lvStream);

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

procedure TMyClientContext.OnDiscounnected;
begin
  inherited;
  if FdmMain <> nil then
  begin
    FdmMain.Free;
    FdmMain := nil;
  end;
end;

end.
