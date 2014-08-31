unit ufrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, safeLogger;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FSafeLogger: TSafeLogger;
  public
    destructor Destroy; override;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

destructor TForm1.Destroy;
begin
  FSafeLogger.Enable :=false;
  FreeAndNil(FSafeLogger);
  inherited Destroy;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  sfLogger.logMessage('记录日志');
  FSafeLogger.logMessage('记录日志', 'DEBUG');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowMessage(sfLogger.getStateINfo);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  sfLogger.setAppender(TStringsAppender.Create(self.Memo1.Lines));
  sfLogger.AppendInMainThread := true;
  sfLogger.start;

  FSafeLogger := TSafeLogger.Create;
  FSafeLogger.setAppender(TLogFileAppender.Create(True));
  FSafeLogger.start;


end;

end.
