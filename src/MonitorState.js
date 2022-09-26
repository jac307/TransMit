
export const setMaterials = ol => m => () => ol.setMaterials(m);

export const mapVidTextToMat = m => vt => () => m.materials.Material.map = vt;

export const mapMatToObj => o => n => m => () => o.children[n].material = m.materials.Material;
