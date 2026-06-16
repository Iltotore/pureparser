# PureParser examples

Here are some examples you can play with. You can directly use them [online](https://iltotore.github.io/pureparser/examples/)
or locally by running the following command:

```sh
./millw examples.<example> <file>
```

where `example` is the name of the example file (e.g `json`) and `file` the file to read (e.g `examples/sample.json`). 

Notes:
- For Windows users, use `./millw.bat` instead.
- You can use `/dev/stdin` to read from standard input so you can try custom inputs by doing `echo "..." | mill examples.<example> /dev/stdin`