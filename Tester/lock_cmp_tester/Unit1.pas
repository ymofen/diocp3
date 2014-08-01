unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

/// <summary>
///   对比target 和cmp_val，如果一样设置成new_val
///     始终返回原值
/// </summary>
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean): Boolean; overload;
asm
{$ifdef win32}
  lock cmpxchg [ecx], dl
{$else}
.noframe
  mov rax, rcx
  lock cmpxchg [r8], dl
{$endif}
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  lvSending:Boolean;
  lvOld:Boolean;
begin
  lvSending := False;

  lvOld := lock_cmp_exchange(False, True, lvSending) = False;

  ShowMessage(BoolToStr(lvOld, true));
  ShowMessage(BoolToStr(lvSending, true));


end;

end.
