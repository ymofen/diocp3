{*******************************************************}
{                                                       }
{       u_class_http_packet                             }
{      Create By£º Locet 2014/8/28                      }
{                                                       }
{*******************************************************}
unit u_class_http_iocp_server;

interface

uses
  System.IniFiles, uIOCPCentre, System.Classes, u_class_http_packet;

type
  TRecvHttpClientContext = procedure(pvClientContext: THttpClientContext;
    http_request: THttpRequest) of object;

  THttpIOCPServer = class(TIOCPConsole)
  private
    FOnRecvHttpClientContext: TRecvHttpClientContext;

  public
    procedure DoRecvClientContext(pvClientContext: TIOCPCoderClientContext;
      pvObject: TObject);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OnRecvHttpClientContext: TRecvHttpClientContext
      read FOnRecvHttpClientContext write FOnRecvHttpClientContext;

  end;

implementation

{ THttpIOCPServer }

constructor THttpIOCPServer.Create(AOwner: TComponent);
begin
  inherited;
  OnDataObjectReceived := DoRecvClientContext;
end;

destructor THttpIOCPServer.Destroy;
begin

  inherited;
end;

procedure THttpIOCPServer.DoRecvClientContext(pvClientContext
  : TIOCPCoderClientContext; pvObject: TObject);
begin
  if Assigned(FOnRecvHttpClientContext) then
    FOnRecvHttpClientContext(pvClientContext as THttpClientContext,
      pvObject as THttpRequest);
end;

end.
