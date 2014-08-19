unit uMyClientContext;

interface

uses
  uIOCPCentre, udmMain, Classes, SimpleMsgPack, SysUtils;

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
  lvMsgPack:TSimpleMsgPack;
  lvStream:TStream;
begin
  try
    lvStream :=TStream(pvDataObject);
    lvStream.Position := 0;
    
    lvMsgPack.DecodeFromStream(lvStream);

    dmMain.Execute(lvMsgPack.I['cmd.index'], );
  except
    on E:Exception do
    begin
            
    end;
  end;

end;

procedure TMyClientContext.OnConnected;
begin
  inherited;
  FdmMain := TdmMain.Create(nil);
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
