econfd - An erlang conf.d 
==========================

This erlang library enables to check/monitor if some files/resources changed over time and notify some handler module about that:

- Watch local file system files (notify when modified ts changes)
- Watch http endpoint (notify when content MD5 changes)
- Watch S3 object (notify when S3 last-modified changes)

The handlers can be setup by configuration (app.config) or programmatically

Installation
-----------

Add the dependency to you rebar.config
 
```erlang
{deps, [
    ...
    {econfd,     ".*", {git, "git://github.com/posilva/econfd.git",                         {branch, "master"   }}},
    ...
]}.
```

Usage
-----

Configure using app.config:

```erlang
 {econfd, [
        {handlers, [
            %%{HandlerName (UNIQUE), file | http | s3, CallbackModule, [{interval_ms, 10000}, {init_delay, 5000}, {Specific, Options}]},
            {file_example_name, file, file_example_handler, [
                {interval_ms, 10000},
                {init_delay, 5000},
                {file_path, "FILE_PATH"}
            ]},
            {http_example_name, http, http_example_handler, [
                {interval_ms, 5000},
                {url, "URL_HTTP_TO_GET"}
            ]},
            {s3_example_name, s3, s3_example_handler, [
                {interval_ms, 5000},
                {s3_bucket, "S3_BUCKET_NAME"},
                {s3_key, "S3_OBJECT_KEY"},
                {aws_secret_key, "AWS_SECRET_KEY"},
                {aws_access_key, "AWS_ACCESS_KEY"}
            ]}
        ]}
    ]}
```

Or, programmatically:

```erlang
add_watcher(dynamic_example_name, 
            file, 
            dynamic_example_handler, 
            [{interval_ms, 10000},
            {init_delay, 5000},
            {file_path, "FILE_PATH"}]).
GitHub::Markup.render(file, File.read(file))
```

Contributing
------------

The usual thing! 

