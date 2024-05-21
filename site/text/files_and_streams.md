# Files and Streams

Uiua has support for reading and writing files from the filesystem. It has helper functions for reading and writing entire files, as well as a stream abstraction for processing files in chunks. This stream abstraction extends to other input/output sources, such as network sockets.

## Reading and Writing Entire Files

Loading an entire file into an array is very simple.
For demonstration purposes, this website has a built-in file called `example.txt` that we will work with.

We can read an entire file as a string with [`&fras`]() (file read all string).

```
&fras "example.txt"
```

If we instead want to read the file into an array of bytes, we can use [`&frab`]() (file read all bytes).

```
&frab "example.txt"
```

To write an entire array to a file, we can use [`&fwa`]() (file write all). The type of the array determines whether the file is written as text or binary.

```
&fwa "file.txt" "Hello, world!"
```

Each editor on this site has a virtual file system, which means that we can read from files after they have been written to. This is useful for testing file writing functions.

```
&fwa "file.bin" ⇡10
&frab "file.bin"
```

## Streams

Stream are an abstraction for sending and receiving data in chunks from some source. Similar concepts exist in many programming languages, inluding Uiua.

In this tutorial, we will focus on using streams to interact with files. However, Uiua also uses streams for other input/output sources, particularly network sockets.

## Reading Streams

Streams in Uiua are passed around as *handles* - boxed integer values that can be used to look up the stream in the interpreter. Functions that create streams return these handles, and they attach some metadata about what kind of stream it is for debugging purposes.

We can use [`&fo`]() (file open) to open a file for reading. This function returns a stream handle.

```
&fo "example.txt"
```

There are a few functions for reading from streams. The most basic two are [`&rs`]() (read string) and [`&rb`]() (read bytes). These functions read at most a certain number of bytes from the stream and return them as a character or byte array, respectively.

Here, we open a file with [`&fo`]() and then read 10 bytes from it. [`&rb`]() simply puts the bytes in an array, while [`&rs`]() converts them to a string.
```
&rb 10 &fo "example.txt"
&rs 10 &fo "example.txt"
```

If we pass [`infinity`]() as the number of bytes to read, the stream will read until the end of the file. This is functionally equivalent to [`&fras`]() or [`&frab`]().

```
&rs ∞ &fo "example.txt"
```

If we want to read up until some delimiter, we can use [`&ru`]() (read until). This function reads from the stream until it encounters a certain sequence of characters or bytes, and returns everything up to that point.
If the delimiter is not found, the function will read until the end of the stream.

```
&ru "file" &fo "example.txt"
```

In general, you only need to use streams to read from a file if the file is too large to fit in memory. For most use cases, [`&fras`]() and [`&frab`]() are sufficient.

## Writing Streams

The [`&w`]() (write) function writes a character or byte array to a stream. It takes the data to write and the stream handle as arguments.

```
&w "Hello, world!" &fc "file.txt"
```

But wait, if we try to read from the file now, it is empty! 

```
&w "Hello, world!" &fc "file.txt"
&fras "file.txt"
```

This is because the writes we have made have not been flushed to the file.
Streams should always be closed with [`&cl`]() (close) when they are no longer needed. This will flush any remaining writes to the file and close the file handle.

```
&cl &w "Hello, world!" . &fc "file.txt"
&fras "file.txt"
```

Knowing this, we see that the examples in the section above are actually incorrect! We should close the file even after reading from it.

[`under`]() can help here. It will automatically close the stream with [`&cl`]() after the block of code is executed.

```
⍜(&fc "file.txt"|&w "Hello, world!")
&fras "file.txt"
```