{
MiConfigIni 0.1b
=============
Por Tito Hinostroza 29/07/2016

Descripción
===========
Unidad con rutinas de lectura/escritura de propiedades en archivos INI. Permite crear
fácilmente, una ventana de configuración, con las opciones: ACEPTAR y CANCELAR.
Está basado en la librería ConfigFrame, pero a diferencia de esta, aquí las propiedades
no se separan en "frames", sino que todas las propiedades se manejan en un mismo objeto.
Para alamacenar las propiedades, se debe crear un objeto TMiConfigINI. Sin embargo,
la unidad crea por defecto, una isntancia de TMiConfigINI, llamada "iniFile", que toma
como nombre <nombre del proyecto.ini>
Tiene como dependencia a la librería MisUtils.

Por Tito Hinostroza 29/07/2016
}
unit MiConfigINI;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, ExtCtrls, IniFiles, Dialogs,
  Graphics, MisUtils, MiConfigBasic;

type
  { TMiConfigINI }
  {Clase base que es usada para manejar los campos de configuración.}
  TMiConfigINI = class(TMiConfigBasic)
  private
    INIfile    : string;   //archivo XML
    function DefaultFileName: string;
    procedure FileProperty(iniCfg: TIniFile; const r: TParElem; FileToProp: boolean);
  public
    secINI: string;   //sección donde se guardarán los datos en un archivo INI
    property FileName: string read INIfile write INIfile;
    procedure VerifyFile;
    function FileToProperties: boolean; virtual;
    function PropertiesToFile: boolean; virtual;
  public  //Constructor y Destructor
    constructor Create(INIfile0: string);
    destructor Destroy; override;
  end;

var
  iniFile : TMiConfigINI;   //Default INI Config file

implementation
//Funciones de uso interno
function CodeStr(s:string): string;
{Protege a una cadena para que no pierda los espacios laterales si es que los tiene,
porque el el archivo INI se pierden. Además codifica el caracter "=", porque es
reservado en el archvio INI}
begin
  Result := '.'+s+'.';
  Result := StringReplace(Result, '=', #25, [rfReplaceAll]);  //protege caracter
  Result := StringReplace(Result, LineEnding, #26, [rfReplaceAll]);  //protege caracter
end;
function DecodeStr(s:string): string;
{Quita la protección a una cadena que ha sido guardada en un archivo INI}
begin
  Result:=copy(s,2,length(s)-2);
  Result := StringReplace(Result, #25, '=', [rfReplaceAll]);  //protege caracter
  Result := StringReplace(Result, #26, LineEnding, [rfReplaceAll]);  //protege caracter
end;
{ TMiConfigINI }
function TMiConfigINI.DefaultFileName: string;
{Devuelve el nombre pro defecto del archvio de configuración}
begin
  Result := ChangeFileExt(Application.ExeName,'.ini');
end;
procedure TMiConfigINI.VerifyFile;
//Verifica si el archivo INI "FileName" existe. Si no, muestra un mensaje y lo crea.
var
  F: textfile;
begin
  if not FileExists(fileName) then begin
    MsgErr('No INI file found: %s', [fileName]);
    //crea uno vacío para leer las opciones por defecto
    AssignFile(F, fileName);
    Rewrite(F);
    CloseFile(F);
  end;
end;
procedure TMiConfigINI.FileProperty(iniCfg: TIniFile; const r: TParElem; FileToProp: boolean);
{Permite leer o escribir una propiedad en el archivo XML}
var
  n, j: Integer;
  d: Double;
  s: String;
  list: TStringList;
  strlst: TStringList;
  b: Boolean;
  c: TColor;
begin
  case r.tipPar of
  tp_Int, tp_Int_TEdit, tp_Int_TSpinEdit:
    if FileToProp then begin  //lee entero
       Integer(r.Pvar^) := iniCfg.ReadInteger(secINI, r.etiqVar, r.defEnt);
    end else begin
      n := Integer(r.Pvar^);
      iniCfg.WriteInteger(secINI, r.etiqVar, n);
    end;
  //---------------------------------------------------------------------
  tp_Dbl, tp_Dbl_TEdit, tp_Dbl_TFloatSpinEdit:
    if FileToProp then begin
       Double(r.pVar^) := iniCfg.ReadFloat(secINI, r.etiqVar, r.defDbl);
    end else begin
      d := Double(r.Pvar^);
      iniCfg.WriteFloat(secINI, r.etiqVar, d);
    end;
  //---------------------------------------------------------------------
  tp_Str, tp_Str_TEdit, tp_Str_TEditButton, tp_Str_TCmbBox:
    if FileToProp then begin  //lee cadena
       String(r.Pvar^) := DecodeStr(iniCfg.ReadString(secINI, r.etiqVar, '.'+r.defStr+'.'));
    end else begin
      s := String(r.Pvar^);
      iniCfg.WriteString(secINI, r.etiqVar,CodeStr(s));
    end;
  //---------------------------------------------------------------------
  tp_Bol, tp_Bol_TCheckBox, tp_Bol_TRadBut:
    if FileToProp then begin  //lee booleano
      boolean(r.Pvar^) := iniCfg.ReadBool(secINI, r.etiqVar, r.defBol);
    end else begin
      b := boolean(r.Pvar^);
      iniCfg.WriteBool(secINI, r.etiqVar, b);
    end;
  //---------------------------------------------------------------------
  tp_Enum, tp_Enum_TRadBut, tp_Enum_TRadGroup:
    if FileToProp then begin  //lee enumerado como entero
       if r.lVar = 4 then begin  //tamaño común de las variable enumeradas
         Int32(r.Pvar^) := iniCfg.ReadInteger(secINI, r.etiqVar, r.defEnt);
       end else begin  //tamaño no implementado
         msjErr := dic('Enumerated type no handled.');
         exit;
       end;
    end else begin
      if r.lVar = 4 then begin
        n := Int32(r.Pvar^);   //lo guarda como entero
        iniCfg.WriteInteger(secINI, r.etiqVar, n);
      end else begin  //tamaño no implementado
        msjErr := dic('Enumerated type no handled.');
        exit;
      end;
    end;
  //---------------------------------------------------------------------
  tp_TCol_TColBut:
    if FileToProp then begin  //lee TColor
       TColor(r.Pvar^) := iniCfg.ReadInteger(secINI, r.etiqVar, r.defCol);
    end else begin
      c := Tcolor(r.Pvar^);
      iniCfg.WriteInteger(secINI, r.etiqVar, c);
    end;
  tp_StrList:
    if FileToProp then  begin //lee TStringList
      list := TStringList(r.Pvar^);
      iniCfg.ReadSection(secINI+'_'+r.etiqVar, list);
      //decodifica cadena
      for n:=0 to list.Count-1 do list[n] := DecodeStr(list[n]);
    end else begin
      strlst := TStringList(r.Pvar^);
      iniCfg.EraseSection(secINI+'_'+r.etiqVar);
      for j:= 0 to strlst.Count-1 do begin
        iniCfg.WriteString(secINI+'_'+r.etiqVar,
                           CodeStr(strlst[j]),'');
      end;
    end;
  tp_StrList_TListBox:
    if FileToProp then begin //lee TStringList
       list := TStringList(r.Pvar^);
       iniCfg.ReadSection(secINI+'_'+r.etiqVar, list);
       //decodifica cadena
       for n:=0 to list.Count-1 do list[n] := DecodeStr(list[n]);
    end else begin
       strlst := TStringList(r.Pvar^);
       iniCfg.EraseSection(secINI+'_'+r.etiqVar);
       for j:= 0 to strlst.Count-1 do begin
         iniCfg.WriteString(secINI+'_'+r.etiqVar,
                            CodeStr(strlst[j]),'');
       end;
    end;
  else  //no se ha implementado bien
    msjErr := dic('Design error.');
    exit;
  end;
end;
function TMiConfigINI.FileToProperties: boolean;
{Lee de disco las propiedades registradas
Si encuentra error devuelve FALSE, y el mensaje de error en "MsjErr".}
var
  r: TParElem;
  iniCfg: TIniFile;
begin
  if not FileExists(fileName) then begin
    MsjErr := dic('INI file does not exist.');  //errro
    exit(false);  //para que no intente leer
  end;
  //Asume error por defecto
  Result := false;
  MsjErr := dic('Error reading INI file: %s', [fileName]);
  try
    iniCfg := TIniFile.Create(fileName);
    for r in listParElem do begin
      FileProperty(iniCfg, r, true);
    end;
    //Terminó con éxito. Actualiza los cambios
    if OnPropertiesChanges<>nil then OnPropertiesChanges;
    Result := true;  //sin error
    MsjErr := '';    //sin error
  finally
    iniCfg.Free;      //libera
  end;
end;
function TMiConfigINI.PropertiesToFile: boolean;
{Guarda en disco las propiedades registradas
Si encuentra error devuelve FALSE, y el mensaje de error en "MsjErr".}
var
  r: TParElem;
  iniCfg: TIniFile; //
begin
  Result := false;
  MsjErr := dic('Error writing INI file: %s', [fileName]);
  try
    If FileExists(fileName)  Then  begin  //ve si existe
       If FileIsReadOnly(fileName) Then begin
          MsjErr := dic('INI file is only read.');
          exit(false);
       End;
    End;
    iniCfg := TIniFile.Create(fileName);
    for r in listParElem do begin
      FileProperty(iniCfg, r, false);
    end;
    Result := true;  //sin error
    MsjErr := '';    //sin error
  finally
    iniCfg.Free;      //libera
  end;
end;
//Constructor y Destructor
constructor TMiConfigINI.Create(INIfile0: string);
begin
  inherited Create;
  INIfile := INIfile0;
  secINI := 'config';  //sección por defecto en archivo INI
end;
destructor TMiConfigINI.Destroy;
begin
  inherited Destroy;
end;

initialization
  iniFile := TMiConfigINI.Create(iniFile.DefaultFileName);

finalization
  iniFile.Destroy;
end.

