program UDPSERVERTester;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {Form1},
  Un_LzUDP in 'Un_LzUDP.pas',
  Winsock2 in 'Winsock2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
