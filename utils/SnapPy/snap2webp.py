#! /usr/bin/env python3

"""
read a variable of a snap.nc file and convert it to a series of images in webp format
the images are stored per timestep
resolution is 4 pixels per grid cell of the whole grid, to get proper rectangles
no background images, the images should be used as overlays on a map, so transparent background
bounding box corners should be written in a json file
projection should be web-mercator, EPSG:3857
currently used colorscale should be written down, too
"""

import argparse
import os
import sys

import matplotlib

matplotlib.use("Agg")  # Use a non-interactive backend for plotting
import json
import logging

import cartopy.crs as ccrs
import matplotlib.pyplot as plt
import numpy as np
import xarray as xr
from cartopy.crs import Globe
from matplotlib.colors import BoundaryNorm

logger = logging.getLogger(__name__)


def center_to_edges(center_array):
    """Convert an array of center coordinates to edge coordinates.

    :param center_array: 1D array of center coordinates
    :return: 1D array of edge coordinates
    """
    edges = np.zeros(len(center_array) + 1)
    edges[1:-1] = (center_array[:-1] + center_array[1:]) / 2
    edges[0] = center_array[0] - (center_array[1] - center_array[0]) / 2
    edges[-1] = center_array[-1] + (center_array[-1] - center_array[-2]) / 2
    return edges


def plot_data_to_webp(
    data: xr.DataArray, filename, colormap, colorscale, data_projection
) -> tuple[float, float, float, float]:
    """Plot data with blank background and without any borders to a 4x higher resolution

    :param data: data to plot
    :param filename: output filename for the webp image
    :param colormap: colormap name to use
    :param colorscale: list of color scale boundaries
    :param data_projection: data-projection
    :return: extent of the plotted data in PlateCarree projection (west, east, south, north)
    """
    plt.close("all")  # Close any existing plots to free memory

    fig, axis = plt.subplots(
        1,
        1,
        subplot_kw=dict(projection=data_projection),
        figsize=(
            data.shape[1] * 4 / 100,
            data.shape[0] * 4 / 100,
        ),  # 4 pixels per grid cell, dpi=100
    )
    fig.patch.set_alpha(0)
    axis.axis("off")

    # make data below colorscale[0] transparent
    cmap = plt.get_cmap(colormap)
    norm = BoundaryNorm(colorscale, ncolors=cmap.N, clip=False)
    data_masked = data.where(
        data >= colorscale[0]
    )  # make data below colorscale[0] transparent
    # using data.plot or data.plot.imshow gives a coarse interpolation of the data
    # data_masked.plot(
    #     ax=axis,
    #     transform=data_projection,  # Assuming data is in PlateCarree projection
    #     add_colorbar=False,
    #     add_labels=False,
    #     vmin=colorscale[0],
    #     vmax=colorscale[-1],
    #     cmap=cmap,
    #     norm=norm,
    # )

    # with io.BytesIO() as buffer:  # use buffer memory
    #     plt.savefig(
    #         buffer,
    #         bbox_inches="tight",
    #         transparent=True,
    #         format="webp",
    #         pad_inches=0,
    #     )

    #     buffer.seek(0)
    #     image = buffer.getvalue()
    # plt.close("all")

    # with open(filename, "wb") as f:
    #     f.write(image)

    # x_min = float(data["x"].min())
    # x_max = float(data["x"].max())
    # y_min = float(data["y"].min())
    # y_max = float(data["y"].max())
    x_edges = center_to_edges(data["x"].values)
    y_edges = center_to_edges(data["y"].values)

    axis.imshow(
        data_masked.values,
        transform=data_projection,
        cmap=cmap,
        norm=norm,
        extent=[
            # use edges instead of centers
            x_edges[0],
            x_edges[-1],
            y_edges[0],
            y_edges[-1],
        ],
        origin="lower",
        interpolation="nearest",
    )
    plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
    fig.savefig(
        filename, dpi=100, transparent=True, pad_inches=0, pil_kwargs={"lossless": True}
    )

    # img = Image.open(filename)
    # print("saved image size:", img.size)
    # print("data nx, ny:", data.shape)
    # print("extent:", axis.get_extent(crs=data_projection))

    # Return the extent of the plotted data in the output projection
    return axis.get_extent(crs=data_projection), axis.get_extent(crs=ccrs.PlateCarree())


def plot_maps(nc_file, variable, output_dir, colormap, colorscale):
    with xr.open_dataset(nc_file) as ds:
        if variable not in ds.variables:
            raise ValueError(f"Variable {variable} not found in {nc_file}")
        times = ds["time"].values
        if "grid_mapping" in ds[variable].attrs:
            proj_name = ds[variable].attrs["grid_mapping"]
            if proj_name in ds.variables:
                proj_var = ds[proj_name]
                if "grid_mapping_name" in proj_var.attrs:
                    projection_CF = proj_var.attrs
                else:
                    raise ValueError(f"grid_mapping_name not found in {proj_name}")
            else:
                raise ValueError(
                    f"grid_mapping variable {proj_name} not found in {nc_file}"
                )
        else:
            projection_CF = {"grid_mapping_name": "latitude_longitude"}

        projection_proj4 = None
        globe = Globe(semimajor_axis=6371000, semiminor_axis=6371000)
        if projection_CF.get("grid_mapping_name") == "lambert_conformal_conic":
            try:
                sp = projection_CF["standard_parallel"]
                if isinstance(sp, (list, tuple)) and len(sp) == 2:
                    standard_parallels = (float(sp[0]), float(sp[1]))
                else:
                    standard_parallels = (float(sp), float(sp))
                central_longitude = float(
                    projection_CF["longitude_of_central_meridian"]
                )
                central_latitude = float(projection_CF["latitude_of_projection_origin"])
                data_projection = ccrs.LambertConformal(
                    globe=globe,
                    central_longitude=central_longitude,
                    central_latitude=central_latitude,
                    standard_parallels=standard_parallels,
                )
                projection_proj4 = f"+proj=lcc +lat_1={standard_parallels[0]} +lat_2={standard_parallels[1]} +lat_0={central_latitude} +lon_0={central_longitude} +R=6371000 +units=m +towgs84=0,0,0 +no_defs"
            except Exception as e:
                logger.error(f"Error converting {projection_CF}: {e}", file=sys.stderr)
                raise RuntimeError(f"Error converting {projection_CF}: {e}")

        elif projection_CF.get("grid_mapping_name") == "latitude_longitude":
            # PlateCarree with that globe (Cartopy version must support globe argument)
            data_projection = ccrs.PlateCarree(
                globe=globe
            )  # Use PlateCarree for lat/lon data
            projection_proj4 = "+proj=longlat +R=6371000 +towgs84=0,0,0 +no_defs"
        else:
            logger.warning(
                f"Warning: Unrecognized projection {projection_CF.get('grid_mapping_name')}, defaulting to PlateCarree"
            )
            data_projection = ccrs.PlateCarree(
                globe=globe
            )  # Use PlateCarree for lat/lon data

        # timesteps -> file mapping
        inventory = {
            "variable": variable,
            "units": ds[variable].attrs.get("units", ""),
            "colormap": colormap,
            "colorscale": colorscale,
            "projection_proj4": projection_proj4,
        }

        # plot the data for each time step
        timestep_to_file = dict()
        for t_idx, dt in enumerate(times):
            dtx = dt.astype("datetime64[us]").astype("O")  # Convert to Python datetime
            data = ds[variable].isel(time=t_idx)
            filename = f"{variable}_{dtx.strftime('%Y%m%dT%H%M%SZ')}.webp"
            filepath = os.path.join(
                output_dir, f"{variable}_{dtx.strftime('%Y%m%dT%H%M%SZ')}.webp"
            )
            timestep_to_file[dtx.strftime("%Y-%m-%dT%H:%M:%SZ")] = filename
            image_extent, extent_pc = plot_data_to_webp(
                data, filepath, colormap, colorscale, data_projection
            )

        bbox = {
            "west": extent_pc[0],
            "east": extent_pc[1],
            "south": extent_pc[2],
            "north": extent_pc[3],
        }
        image_extent = {
            "min_x": image_extent[0],
            "max_x": image_extent[1],
            "min_y": image_extent[2],
            "max_y": image_extent[3],
        }
        inventory["bounding_box"] = bbox
        inventory["image_extent"] = image_extent
        inventory["timesteps"] = timestep_to_file
        with open(os.path.join(output_dir, f"variable_{variable}.json"), "w") as f:
            json.dump(inventory, f, indent=4)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="convert a snap.nc output-file to webp images, should be run after snapAddToa"
    )
    parser.add_argument("--nc", help="snap.nc filename", required=True)
    parser.add_argument("--var", help="variable name to convert", required=True)
    parser.add_argument("--output-dir", help="output-directory", required=True)
    parser.add_argument("--colormap", help="colormap name to use", default="gray")
    parser.add_argument(
        "--colorscale",
        help="comma-separated colorscale",
        default="100,300,1000,3000,10000,30000,100000,300000,1000000,3000000",
    )
    args = parser.parse_args()

    scale = args.colorscale.split(",")
    if len(scale) < 2:
        raise ValueError("colorscale must have at least 2 values")
    scale = [float(x) for x in scale]

    plot_maps(
        nc_file=args.nc,
        variable=args.var,
        output_dir=args.output_dir,
        colormap=args.colormap,
        colorscale=scale,
    )
