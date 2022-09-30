
export const preloadMaterials = m => () => m.preload()

export const mapVidTextToMat = m => vt => () => m.materials.Material.map = vt;

export const mapMatToObj = o => n => m => () => o.children[n].material = m.materials.Material;
