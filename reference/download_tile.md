# Download a single GFC tile

Downloads a single GFC tile and verifies the download.

## Usage

``` r
download_tile(tile_url, local_path, timeout = 1000)
```

## Arguments

- tile_url:

  URL of the tile to download

- local_path:

  Local path to save the tile

- timeout:

  Timeout in seconds for download

## Value

Integer: 0 if successful, 1 if download failed, 2 if verification failed
