
// export const ifVidStatement = v = vt = () => {if( v.readyState === video.HAVE_ENOUGH_DATA ) vt.needsUpdate	= true;}

export const preloadMaterials = m => () => m.preload()

export const mapVidTextToMat = m => vt => () => m.materials.Material.map = vt;

export const mapMatToObj = o => n => m => () => o.children[n].material = m.materials.Material;

export const matTransparency = o => n => () => o.children[n].material.transparent = true;

export const matOpacity = o => n => () => o.children[n].material.opacity = 0.5;

export const matOpacity2 = m => () => m.materials.Material.opacity = 0.5;


// object.children[0].material.transparent = true;
// object.children[0].material.opacity = 0.5;
