unit uMyClientContext;

interface

uses
  uIOCPCentre;

type
  TMyClientContext = class(uIOCPCentre.TIOCPClientContext)
  private
    FPullerID:String;
  public
    
  end;

implementation

end.
