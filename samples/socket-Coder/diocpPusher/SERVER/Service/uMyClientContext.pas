unit uMyClientContext;

interface

uses
  uIOCPCentre, SimpleMsgPack, Classes, uZipTools, SysUtils;

type
  TMyClientContext = class(TIOCPCoderClientContext)
  private
    FPullerID:String;
    procedure registerPuller(pvMsgPack:TSimpleMsgPack);
    procedure unRegisterPuller();
  protected
    /// <summary>
    ///   on received a object
    /// </summary>
    /// <param name="pvDataObject"> (TObject) </param>
    procedure dataReceived(const pvDataObject:TObject); override;
  public

    
  end;

implementation

uses
  uPullerMananger;

{ TMyClientContext }

procedure TMyClientContext.dataReceived(const pvDataObject: TObject);
var
  lvMsgPack:TSimpleMsgPack;
  lvStream :TStream;
  lvStream2:TMemoryStream;
  lvResult:Boolean;
  lvCMDIndex:Integer;
begin
  lvMsgPack := TSimpleMsgPack.Create;
  try
    try
      lvStream := TStream(pvDataObject);
      lvStream.Position := 0;

      lvMsgPack.DecodeFromStream(lvStream);

      lvCMDIndex := lvMsgPack.I['cmd.index'];
      case lvCMDIndex of
        1: registerPuller(lvMsgPack);
        2: unRegisterPuller;
      else
        begin
          raise Exception.CreateFmt('Î´ÖªµÄÃüÁî[%d]', [lvCMDIndex]);
        end;
      end;
      
      // write result info
      lvMsgPack.B['__result.result'] := True;
    except
      on E:Exception do
      begin
        lvMsgPack.B['__result.result'] := false;
        lvMsgPack.S['__result.msg'] := e.Message;
      end;
    end;

    lvStream.Size := 0;
    lvMsgPack.EncodeToStream(lvStream);
    lvStream.Position := 0;

    // send to client
    self.writeObject(lvStream);
  finally
    lvMsgPack.Free;
  end;
end;

procedure TMyClientContext.registerPuller(pvMsgPack: TSimpleMsgPack);
begin
  FPullerID := pvMsgPack.S['params.ID'];
  pullerMananger.registerPuller(FPullerID, self);  
end;

procedure TMyClientContext.unRegisterPuller;
begin
  pullerMananger.removePuller(FPullerID);
end;

end.
