
// export const ifVidStatement = v = vt = () => {if( v.readyState === video.HAVE_ENOUGH_DATA ) vt.needsUpdate	= true;}

export const preloadMaterials = m => () => m.preload()

export const mapVidTextToMat = m => vt => () => m.materials.Material.map = vt;

export const mapMatToObj = o => n => m => () => o.children[n].material = m.materials.Material;
