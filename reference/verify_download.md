# Verify downloaded GFC tile file integrity

Checks if the downloaded file size matches the remote file size.

## Usage

``` r
verify_download(tile_url, local_path)
```

## Arguments

- tile_url:

  URL of the remote file

- local_path:

  Path to the local downloaded file

## Value

Integer: 0 if verification passed, 3 if failed
