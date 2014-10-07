MisUtils 0.1
============

Useful routines for Lazarus, for showing messages and developing multilingual applications.

Inside this unit, we have this functions for showing messages:

* MsgBox() -> Shows a box with a message. Similar a ShowMessage.
* MsgExc() -> Shows a box with a message and an Icon of Exclamation. 
* MsgErr() -> Shows a box with a message and an Icon of Error. 

* MsgYesNo() -> Prompt for a Yes/No answer.
* MsgYesNoCancel() -> Prompt for a Yes/No answer, with a cancel option.

Moreover, this unit includes a diccionary for translating messages. For to use it, are  declared the next fileds:

* TranslateMsgs -> Flag for to enable the translation of  messages used with MsgBox(), MsgExc() and MsgErr().
* dic() -> Translates one string using the internal dictionary.
* dicSet() -> Adds or replace an entry of the internal dictionary.
* dicDel() -> Cleans an entry of the internal dictionary.
* dicClear() -> Removes all entries of the internal dictionary.


