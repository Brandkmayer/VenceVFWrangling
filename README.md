# VenceVFWrangling

Walkthrough for retrieving virtual fence coordinates from HerdManager Application and creating polygons of management zones.

## getting fence coordinates

Select the Edit option for the virtual fence of interest on herdmanager

![Herd Selection](Images/HerdManagerSelectHerdForVirtualFenceManagement.png)

Copy the GPS coordinates into a notepad.

![Clip of Coordiante location](Images/Pastedimage20221213113838.png)

The coordinates should paste into the note in a similar fashion as below.

![RAW Coordinates](Images/Pastedimage20221213103534.png)

This is the outline of the entire shape of the fence. "Of note, the total vertices of the virtual fence appear to have no limit but the active fences are restricted to 16." This shapefile doesn't include the active fences. You'll need tag the active fences manually in the order listed in the "info tab" of the edit fence option.

![Coordinate Organization](Images/Pastedimage20221220153301.png)

<p style="text-align:center">

Note the active fences locations along the fence boundary, noted with "V"s

</p>

<p style="text-align:center">

🠋

</p>

![Coordinates and Fgroups](Images/Pastedimage20221213110947.png)

<p style="text-align:center">

Create a new column called "Fgroup" to assigning tags for each active fence. There may just be one, but this still needs to be tagged.

</p>

### Prep

All virtual fences that are planned to be used should be processed in this method and stored in a file for RAW virtual fences upon completion.

## Storing metadata

![Metadata](Images/Pastedimage20221213102230.png)

You'll need to manually store meta data associated with each fence. Pertinent information includes the Virtual Herd Name, the Fence Name, when the fence turned on and off, the width of the boundaries, and the type of fence used (exclosure or enclosure). Fence metadata should be added when the on and off date are uploaded to a herd.

| Herd                  | Fence                | On             | Off            | Shock | Sound | Objective | ExclType |
|---------|---------|---------|---------|---------|---------|---------|---------|
| Small Herd 2022-06-22 | Vence Training Day 1 | 6/28/2022 0:00 | 6/29/2022 0:00 | 25    | 0     | Training  | In       |

### Future Changes

Vence has verified a future change to this work flow.

The step connecting RAW coordinated to their perspective virtual fence will be included in the details section of the "Edit fence" option. 

This will allow the user to collect this information regardless of whether or not the fence is uploaded or in the process of being uploaded to a herd. 

This script will be adapted to that process once the update to herd manager occurs.