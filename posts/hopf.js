let renderer, camera, controls, scene;

function init() {
    let canvas = document.getElementById("c");
    camera = new THREE.PerspectiveCamera(75, window.innerWidth/window.innerHeight, 0.1, 1000);
    renderer = new THREE.WebGLRenderer();
    scene = new THREE.Scene();

    renderer.setSize(window.innerWidth, window.innerHeight);
    document.body.appendChild(renderer.domElement);
    controls = new THREE.OrbitControls(camera, renderer.domElement);


    let hyper = hyperSphere(1, 10, 10, 10)
    hyper.forEach((v) => plotPoint(project(v)))
    var spotLight = new THREE.SpotLight(0xffffff);
    spotLight.position.set(-0, 30, 60);
    scene.add(spotLight);
    camera.position.z = 10;
    animate()
}

function colorize(v3) {
    let n = v3.clone().normalize()
    let color = new THREE.Color(n.x, n.y, n.z)
    return color
}

function animate() {
    requestAnimationFrame(animate)
    controls.update()
    renderer.render(scene, camera)
}

function hyperSphere(radius, xSegments, ySegments, zSegments) {
    let vertices = []
    let vertex = new THREE.Vector4()
    for (let ix = 0; ix <= xSegments; ix++) {
        for (let iy = 0; iy <= ySegments; iy++) {
            for (let iz = 0; iz <= zSegments; iz++) {
                let psi = (iz/zSegments) * Math.PI
                let theta = (ix/xSegments) * Math.PI
                let phi = (iy/ySegments) * 2 * Math.PI
                vertex.x = radius * Math.sin(psi) * Math.sin(theta) * Math.cos(phi)
                vertex.y = radius * Math.sin(psi) * Math.sin(theta) * Math.sin(phi)
                vertex.z = radius * Math.sin (psi) * Math.cos(theta)
                vertex.w = radius * Math.cos(psi)
                vertices.push(vertex.clone());
            }

        }
    }
    return vertices
}

// η(q)=(0,2wy+2xz,2yz−2wx,w2+z2−x2−y2).
function fiber(v3, phi) {
    let alpha = Math.sqrt((1 + v3.z)/2)
    let beta = Math.sqrt((1 - v3.z)/2)
    let theta = Math.atan((-v3.x)/v3.y) - phi
    for ()
}

function project(v4) {
    let v3 = new THREE.Vector3()
    v3.x = (v4.x / (1 - v4.w))
    v3.y = (v4.y / (1 - v4.w))
    v3.z = (v4.z / (1 - v4.w))
    return v3;
}

function plotPoint(v3) {
    let geometry = new THREE.SphereGeometry(0.25, 10, 10)
    geometry.translate(v3.x, v3.y, v3.z)
    let material = new THREE.MeshPhongMaterial({color: (colorize(v3))});
    let sphere = new THREE.Mesh( geometry, material );
    scene.add(sphere);
}

// function HyperSphereBufferGeometry(radius, xSegments, ySegments, zSegments) {
//     THREE.BufferGeometry.call(this);

//     this.type = 'HyperSphereBufferGeometry';
//     let index = 0
//     let vertices = []
//     let normals = []
//     let grid = []
//     let indices = []
//     let vertex = new THREE.Vector4()
//     let normal = new THREE.Vector4()
//     for (let ix = 0; ix <= xSegments; ix++) {
//         let xVertices = []
//         for (let iy = 0; iy <= ySegments; iy++) {
//             let yVertices = []
//             for (let iz = 0; iz <= zSegments; iz++) {
//                 let psi = (iz/zSegments) * Math.PI
//                 let theta = (ix/xSegments) * Math.PI
//                 let phi = (iy/ySegments) * 2 * Math.PI
//                 vertex.x = radius * Math.sin(psi) * Math.sin(theta) * Math.cos(phi)
//                 vertex.y = radius * Math.sin(psi) * Math.sin(theta) * Math.sin(phi)
//                 vertex.z = radius * Math.sin (psi) * Math.cos(theta)
//                 vertex.w = radius * Math.cos(psi)
//                 console.log(vertex.lengthSq())
//                 vertices.push(vertex.x, vertex.y, vertex.z, vertex.w);
//                 normal.copy( vertex ).normalize();
//                 normals.push( normal.x, normal.y, normal.z, vertex.w);
//                 yVertices.push(index++)
//             }
//             xVertices.push(yVertices)

//         }
//         grid.push(xVertices)
//     }

//     for (let ix = 0; ix < xSegments; ix ++) {
//         for (let iy = 0; iy < ySegments; iy ++) {
//             for (let iy = 0; iz < ySegments; iz ++) {

//                 var a = grid[ix][iy + 1];
//                 var b = grid[ix][iy];
//                 var c = grid[ix + 1][iy];
//                 var d = grid[ix + 1][iy + 1];

//                 if (ix !== 0) indices.push(a, b, d);
//                 if (ix !== xSegments - 1) indices.push(b, c, d);
//             }
//         }

//     }
//     this.setIndex(indices);
//     this.setAttribute('position', new THREE.Float32BufferAttribute(vertices, 4))
//     this.setAttribute('normal', new THREE.Float32BufferAttribute(normals, 4));
// }

// HyperSphereBufferGeometry.prototype = Object.create( THREE.BufferGeometry.prototype );
// HyperSphereBufferGeometry.prototype.constructor = HyperSphereBufferGeometry;

init()
