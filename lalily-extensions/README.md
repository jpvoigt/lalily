"private" extensions to lalily
==============================

All files in this folder with a name ending with .ly are included by lalily.

If you want to organize your extensions in a "private" git repository, you can do so: this folder - except this README.md file - is .gitignore'd. 
And you are always free to fork your own branch with your own extensions, if you are confident with git. 

But be careful not to publish private information, when you publish your branch. This can be the case, if you for example have templates including some personal identification.
Or one might build an extension function using some credentials to fetch data from a database using guile-dbi, for example to fetch names or music.

