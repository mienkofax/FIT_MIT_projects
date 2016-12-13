function NURBS(canvas1id, canvas2id, scale, n, k, ptX, ptY, ptW, pt){
	var ptWtmp = ptW;
	input1 = document.getElementById("P1");
	input1.addEventListener('change', edit1, false);
	input1.addEventListener('mousemove', edit1, false);
	input2 = document.getElementById("P2");
	input2.addEventListener('change', edit2, false);
	input2.addEventListener('mousemove', edit2, false);

	cmp = document.getElementById("cmp");
	cmp.addEventListener('click', compare2, false);

	var PxOld = ptX,
		PyOld = ptY,
		PtWOld = ptW,
		PtOld = pt;

	var oldN = n;
	var oldK = k;
	var zobrazit = true;

var canvas, canvas2, ctx, ctx2, w,h,h1, d,d2,  dragId = -1, dragId2 = -1;
var n1 = n+1,  nti = n+k+1,  N;
var iColor = ["#f00000","#00f000","#0000f0","#00f0f0","#f0f000","#f000f0","#090909"];
var Px = new Float64Array(ptX),
    Py = new Float64Array(ptY),
    Wi = new Float64Array(ptWtmp),
    ti = new Float64Array(pt);
   canvas = document.getElementById(canvas1id);
   ctx = canvas.getContext("2d");
   canvas.addEventListener('mousemove', drag, false);
   canvas.addEventListener('touchmove', drag, false);
   canvas.addEventListener('mousedown', start_drag, false);
   canvas.addEventListener('mouseup', stop_drag, false);
   canvas.addEventListener('touchstart', start_drag, false);
   canvas.addEventListener('touchend', stop_drag, false);
   canvas2 = document.getElementById(canvas2id);
   ctx2 = canvas2.getContext("2d");
   canvas2.addEventListener('mousemove', drag2, false);
   canvas2.addEventListener('touchmove', drag2, false);
   canvas2.addEventListener('mousedown', start_drag2, false);
   canvas2.addEventListener('mouseup', stop_drag2, false);
   canvas2.addEventListener('touchstart', start_drag2, false);
   canvas2.addEventListener('touchend', stop_drag2, false);
   window.addEventListener('resize', resize, false);
   var to = ti[0], dt = ti[nti-1]-to;
   for (var i = 0; i < nti; i++) ti[i] = (ti[i]-to)/dt;
   resize();

  function edit1(ev) {
	  var ptWtmp = ptW;
	  ptWtmp[1] = document.getElementById("P1").value;
	  Wi = new Float64Array(ptWtmp);
	  document.getElementById("L1").innerHTML = document.getElementById("P1").value;
	  drawFun();
	  drawSpline();
  }

  function edit2(ev) {
	  var ptWtmp = ptW;
	  ptWtmp[2] = document.getElementById("P2").value;
	  Wi = new Float64Array(ptWtmp);
	  document.getElementById("L2").innerHTML = document.getElementById("P2").value;
	  drawFun();
	  drawSpline();
  }


function drawFun(){
  ctx2.clearRect(0,0, w, h);
  ctx2.lineWidth = d;
  var step = (ti[nti-1]-ti[0])/w;
  for (var j = 0; j < nti*w; j++) N[j] = 0;
  var i1 = 0, t = ti[0];
  for (var l = 0; l < w; l++){
   while (t >= ti[i1] ) i1++;
   var i = i1-1, ntil = nti*l;
   N[i + ntil] = 1;
   for (var m = 2; m <= k; m++){        //  basis functions calculation
    var jb = i-m+1;  if (jb < 0) jb = 0;
    for (var j = jb; j <= i; j++)
     N[j + ntil] = N[j + ntil]*(t - ti[j])/(ti[j+m-1] - ti[j]) +
      N[j+1 +ntil]*(ti[j+m] - t)/(ti[j+m] - ti[j+1]);}
   var sW = 0;
   for (var m = 0; m < n1; m++) sW += Wi[m]*N[m + ntil];
   for (var m = 0; m < n1; m++) N[m + ntil] = Wi[m]*N[m + ntil]/sW;
   t += step;}
  for (var j = 0; j < n1; j++){
   t = ti[0];
   ctx2.strokeStyle = iColor[j % 7];
   ctx2.beginPath();  ctx2.moveTo(w*t, h - h*N[j]);
   for (var l = 1; l < w; l++){
    t += step;
    ctx2.lineTo(w*t, h - h*N[j + nti*l]);
   }
   ctx2.stroke();
  }
  for (var l = k; l <= n1; l++){
   ctx2.strokeStyle = iColor[(l-k) % 7];
   ctx2.beginPath();  ctx2.moveTo(w*ti[l-1], 1);  ctx2.lineTo(w*ti[l], 1);
   ctx2.stroke();
  }
  ctx2.strokeStyle = "black";
  var xo = .5*w/n1;
  for (var i = 0; i < n1; i++)
    ctx2.strokeRect(xo + i*w/n1 - d, h - .2*h*Wi[i], d2, d2);
}
function drawSpline(){
  ctx.clearRect(0,0, w, h);
  ctx.lineWidth = d;
  ctx.strokeStyle = "#0000f0";
  ctx.beginPath();  ctx.moveTo(Px[0]*w, h - Py[0]*h);
  for (var i = 1; i < n1; i++) {
	  if (zobrazit)
   		ctx.lineTo(Px[i]*w, h - Py[i]*h);
   //ctx.fillText("data", Px[i]*100, Py[i]*100);
   }
  ctx.stroke();

  //console.log(Px[0]);

  ctx.lineWidth = d2;
  var step = (ti[nti-1]-ti[0])/w;
  var to = Math.floor(((ti[k-1] - ti[0])/step)) + 1;
  var sX = sY = 0, ntii = to*nti;
  for (var m = 0; m < n1; m++){
   var wi = Wi[m]*N[m + ntii];
   sX += Px[m]*wi;  sY += Py[m]*wi;}
  for (var j = k-1; j < n1; j++){
   var t = Math.floor((ti[j+1] - ti[0])/step);
   ctx.strokeStyle = iColor[(j-k+1) % 7];
   ctx.beginPath();  ctx.moveTo(w*sX, h - h*sY);
   for (var i = to; i < t; i++){
    sX = sY = 0;
    ntii += nti;
    for (var m = 0; m < n1; m++){
     var wi = N[m + ntii];
     sX += Px[m]*wi;  sY += Py[m]*wi;}
    ctx.lineTo(w*sX, h1 - h*sY);
   }
   ctx.stroke();
   to = t;
  }
  ctx.lineWidth = d;
  ctx.strokeStyle = "#0000f0";
  for (var i = 0; i < n1; i++) {
	  if (zobrazit) {
   		ctx.strokeRect(Px[i]*w - d, h - Py[i]*h - d, d2, d2);
   	ctx.font = "18px Arial";
   	ctx.fillText("P" + (i+1), Px[i]*w - d +10 , h - Py[i]*h+5);
   }
   //console.log(i + " " + (Px[i]) + " : " + (Py[i]));
   }
}
function resize(){
   h = w = Math.round(document.getElementById("center-box").clientWidth-30);
   h1 = h-1;
   d = Math.max(1, Math.round(w / 250));  d2 = d+d;
   canvas.width = w;  canvas.height = h;
   canvas2.width = w; canvas2.height = h;
   N = new Float64Array(nti*w);
   drawFun();
   drawSpline();
}
function drag(ev){
  if (dragId < 0) return;
  var c = getXY(ev, canvas);
  Px[dragId] = c[0];  Py[dragId] = c[1];
  drawSpline();
  ev.preventDefault();
}
function start_drag(ev){
  var c = getXY(ev, canvas);
  var Rmin = 2, r2,xi,yi;
  for (var i = 0; i < n1; i++){
   xi = (c[0] - Px[i]); yi = (c[1] - Py[i]);
   r2 = xi*xi + yi*yi;
   if ( r2 < Rmin ){ dragId = i; Rmin = r2;}}
  Px[dragId] = c[0];  Py[dragId] = c[1];
  drawSpline();
  ev.preventDefault();
}
function stop_drag(ev){
  dragId = -1;
  ev.preventDefault();
}
function drag2(ev){
  if (dragId2 < 0) return;
  var c = getXY(ev, canvas2);
  Wi[dragId2] = c[1]*5;
  drawFun();
  drawSpline();
  ev.preventDefault();
}
function start_drag2(ev){
  var c = getXY(ev, canvas2);
  dragId2 = Math.floor(n1*c[0]);
  Wi[dragId2] = c[1]*5;
  drawFun();
  drawSpline();
  ev.preventDefault();
}
function stop_drag2(ev){
  dragId2 = -1;
  ev.preventDefault();
}
function getXY(ev, cnv){
  if (!ev.clientX) ev = ev.touches[0];
  var rect = cnv.getBoundingClientRect();
  var x = (ev.clientX - rect.left) / w,
      y = (h1 - (ev.clientY - rect.top)) / h;
  return [x, y];
}

reset = document.getElementById("resetL");
reset.addEventListener('click', reset2);

function reset2(e) {
	ptX = PxOld;
	ptY = PyOld;
	Px = new Float64Array(PxOld);
	Py = new Float64Array(PyOld);
	Wi = new Float64Array(PtWOld);
	ti = new Float64Array(PtOld);

	n = oldN;
	k = oldK;
	n1 = n+1;
	nti = n+k+1;

	to = ti[0], dt = ti[nti-1]-to;
	for (var i = 0; i < nti; i++) ti[i] = (ti[i]-to)/dt;
	resize();

	drawFun();
	drawSpline();
}

zob = document.getElementById("zobrazit");
zob.addEventListener('change', zobraz2);

function zobraz2()
{
	zobrazit = zob.checked;
	zob.style.color = "red";
	drawFun();
	drawSpline();
	console.log(zob.checked);
}

function compare2(ev) {
	var data = document.getElementById("nurbs-acc").getElementsByClassName("in")[0].getElementsByClassName("data")[0];
	var data2 = document.getElementById("nurbs-acc").getElementsByClassName("in")[0].getElementsByClassName("data2")[0];

	var vzorove = data.innerHTML.split(',').map(function(n) {return Number(n);});
	var vzorove2 = data2.innerHTML.split(',').map(function(n) {return Number(n);});

	console.log("vzorovy vystup: " + vzorove);

	var res = true;
	if (Math.abs(vzorove2[0]-document.getElementById("P1").value) > 2 || Math.abs(vzorove2[1] - document.getElementById("P2").value > 2))
		res = false;
console.log(Math.abs(vzorove2[0]-document.getElementById("P1").value));

	var k = 0;
	for (var i = 0; i < n1; i++) {
		if (Math.abs(Px[i] - vzorove[k]) > 0.1 || Math.abs(Py[i] - vzorove[k+1]) > 0.1)
			res = false;
	 console.log(i + " " + (Px[i]) + " : " + (Py[i])); //Px[i]*w - d +10 , h - Py[i]*h+5
	 //console.log(i + " " + (Px[i]*w-d) + " : " + (Py[i]*h));
	 k += 2;
	 }

	/* for (var i = 0; i < n1; i++) {
 	 console.log(i + " " + (PxOld[i]) + " : " + (PyOld[i]));
 }*/
	if (res) {
		document.getElementById("res").innerHTML = '<i class="fa fa-check"></i>';
		document.getElementById("vys").style.color = "green";
	}
	else {
		document.getElementById("res").innerHTML = '<i class="fa fa-close"></i>';
		document.getElementById("vys").style.color = "red";
	}
}


novyB = document.getElementById("novy-bod3");
novyB.addEventListener('click', addPointInto);

removeP = document.getElementById("odstranit-bod3");
removeP.addEventListener('click', removePointFrom);

function appendToBuffer(buf1, buf2)
{
	var tmp =  new Float64Array(buf1.length + buf2.length);
	tmp.set(new Float64Array(buf1), 0);
	tmp.set(new Float64Array(buf2), buf1.length);
	return tmp;
}

function removeFromBuffer(buf1)
{
	var tmp = [];
	for (h in buf1) {
		if (h != buf1.length-1)
			tmp.push(buf1[h]);
	}
	return new Float64Array(tmp);
}

function removePointFrom()
{
	var tmp = [];
	var m = 0;
	for (i in pt) {
		if (m != 3)
			//tmp.push(.4+n*0.00013);

			tmp.push(pt[i]);
			m++;
	}
	pt = tmp;

	Px = removeFromBuffer(Px);
	Py = removeFromBuffer(Py);
	Wi = removeFromBuffer(Wi);
	n--;
	ti = new Float64Array(pt);

	n1 = n+1;
	nti = n+k+1;

	to = ti[0], dt = ti[nti-1]-to;
	for (var i = 0; i < nti; i++) ti[i] = (ti[i]-to)/dt;
	resize();

	drawFun();
	drawSpline();
}

function addPointInto() {
	ptWtmp.push(1);

	var tmp = [];
	var m = 0;
	for (i in pt) {
		if (m == 3)
			tmp.push(.4+n*0.00013);

			tmp.push(pt[i]);
			m++;
	}
	pt = tmp;

	Px = appendToBuffer(Px, [0.6]);
	Py = appendToBuffer(Py, [0.4]);
	Wi = new Float64Array(ptWtmp);
	ti = new Float64Array(pt);
	n++;

	n1 = n+1;
	nti = n+k+1;

	to = ti[0], dt = ti[nti-1]-to;
	for (var i = 0; i < nti; i++) ti[i] = (ti[i]-to)/dt;
	resize();

	drawFun();
	drawSpline();
}

} // end
