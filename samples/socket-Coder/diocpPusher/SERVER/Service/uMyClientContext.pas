unit uMyClientContext;

interface

uses
  uIOCPCentre;

type
  TMyClientContext = class(TIOCPCoderClientContext)
  private
    FPullerID:String;
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
begin
  inherited;

end;

end.
