program diocpTcpClientDemo;

uses
  Vcl.Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  AnsiStringTools in '..\diocpCoders\AnsiStringTools.pas',
  JSonStream in '..\diocpCoders\JSonStream.pas',
  superobject in '..\diocpCoders\superobject.pas',
  uCRCTools in '..\diocpCoders\uCRCTools.pas',
  uIOCPJSonStreamDecoder in '..\diocpCoders\uIOCPJSonStreamDecoder.pas',
  uIOCPJSonStreamEncoder in '..\diocpCoders\uIOCPJSonStreamEncoder.pas',
  uMyTypes in '..\diocpCoders\uMyTypes.pas',
  uZipTools in '..\diocpCoders\uZipTools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
