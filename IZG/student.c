/******************************************************************************
 * Projekt - Zaklady pocitacove grafiky - IZG
 * spanel@fit.vutbr.cz
 *
 * $Id:$
 */

#include "student.h"
#include "transform.h"
#include "fragment.h"

#include <memory.h>
#include <math.h>


/*****************************************************************************
 * Globalni promenne a konstanty
 */

/* Typ/ID rendereru (nemenit) */
const int           STUDENT_RENDERER = 1;
float				runTime = 0.0;
const S_Material    MAT_WHITE_AMBIENT = {1.0, 1.0, 1.0, 1.0};
const S_Material    MAT_WHITE_DIFFUSE = {1.0, 1.0, 1.0, 1.0};
const S_Material    MAT_WHITE_SPECULAR = {1.0, 1.0, 1.0, 1.0};

/*****************************************************************************
 * Funkce vytvori vas renderer a nainicializuje jej
 */

S_Renderer * studrenCreate()
{
    S_StudentRenderer * renderer = (S_StudentRenderer *)malloc(sizeof(S_StudentRenderer));
    IZG_CHECK(renderer, "Cannot allocate enough memory");

    /* inicializace default rendereru */
    renderer->base.type = STUDENT_RENDERER;
    renInit(&renderer->base);

    /* nastaveni ukazatelu na upravene funkce */
    /* napr. renderer->base.releaseFunc = studrenRelease; */
    renderer->base.releaseFunc = studrenRelease;
    renderer->base.projectTriangleFunc = studrenProjectTriangle;

    /* inicializace nove pridanych casti */
    renderer->texture = loadBitmap(TEXTURE_FILENAME, &(renderer->width), &(renderer->height));

    return (S_Renderer *)renderer;
}

/*****************************************************************************
 * Funkce korektne zrusi renderer a uvolni pamet
 */

void studrenRelease(S_Renderer **ppRenderer)
{
    S_StudentRenderer * renderer;

    if( ppRenderer && *ppRenderer )
    {
        /* ukazatel na studentsky renderer */
        renderer = (S_StudentRenderer *)(*ppRenderer);

        /* pripadne uvolneni pameti */
        free(renderer->texture);

        /* fce default rendereru */
        renRelease(ppRenderer);
    }
}

/******************************************************************************
 * Nova fce pro rasterizaci trojuhelniku s podporou texturovani
 * Upravte tak, aby se trojuhelnik kreslil s texturami
 * (doplnte i potrebne parametry funkce - texturovaci souradnice, ...)
 * v1, v2, v3 - ukazatele na vrcholy trojuhelniku ve 3D pred projekci
 * n1, n2, n3 - ukazatele na normaly ve vrcholech ve 3D pred projekci
 * x1, y1, ... - vrcholy trojuhelniku po projekci do roviny obrazovky
 */

void studrenDrawTriangle(S_Renderer *pRenderer,
                         S_Coords *v1, S_Coords *v2, S_Coords *v3,
                         S_Coords *n1, S_Coords *n2, S_Coords *n3,
                         int x1, int y1,
                         int x2, int y2,
                         int x3, int y3,
						 S_Coords *tCoords
                         )
{
    /* zaklad fce zkopirujte z render.c */
	int         minx, miny, maxx, maxy;
    int         a1, a2, a3, b1, b2, b3, c1, c2, c3;
    int         s1, s2, s3;
    int         x, y, e1, e2, e3;
    double      alpha, beta, gamma, w1, w2, w3, z;
	double		textureX, textureY;
    S_RGBA      col1, col2, col3, color, textureColor;

    IZG_ASSERT(pRenderer && v1 && v2 && v3 && n1 && n2 && n3);

    /* vypocet barev ve vrcholech */
    col1 = pRenderer->calcReflectanceFunc(pRenderer, v1, n1);
    col2 = pRenderer->calcReflectanceFunc(pRenderer, v2, n2);
    col3 = pRenderer->calcReflectanceFunc(pRenderer, v3, n3);

    /* obalka trojuhleniku */
    minx = MIN(x1, MIN(x2, x3));
    maxx = MAX(x1, MAX(x2, x3));
    miny = MIN(y1, MIN(y2, y3));
    maxy = MAX(y1, MAX(y2, y3));

    /* oriznuti podle rozmeru okna */
    miny = MAX(miny, 0);
    maxy = MIN(maxy, pRenderer->frame_h - 1);
    minx = MAX(minx, 0);
    maxx = MIN(maxx, pRenderer->frame_w - 1);

    /* Pineduv alg. rasterizace troj.
       hranova fce je obecna rovnice primky Ax + By + C = 0
       primku prochazejici body (x1, y1) a (x2, y2) urcime jako
       (y1 - y2)x + (x2 - x1)y + x1y2 - x2y1 = 0 */

    /* normala primek - vektor kolmy k vektoru mezi dvema vrcholy, tedy (-dy, dx) */
    a1 = y1 - y2;
    a2 = y2 - y3;
    a3 = y3 - y1;
    b1 = x2 - x1;
    b2 = x3 - x2;
    b3 = x1 - x3;

    /* koeficient C */
    c1 = x1 * y2 - x2 * y1;
    c2 = x2 * y3 - x3 * y2;
    c3 = x3 * y1 - x1 * y3;

    /* vypocet hranove fce (vzdalenost od primky) pro protejsi body */
    s1 = a1 * x3 + b1 * y3 + c1;
    s2 = a2 * x1 + b2 * y1 + c2;
    s3 = a3 * x2 + b3 * y2 + c3;

    if ( !s1 || !s2 || !s3 ) return;

    /* normalizace, aby vzdalenost od primky byla kladna uvnitr trojuhelniku */
    if( s1 < 0 )
    {
        a1 *= -1;
        b1 *= -1;
        c1 *= -1;
    }
    if( s2 < 0 )
    {
        a2 *= -1;
        b2 *= -1;
        c2 *= -1;
    }
    if( s3 < 0 )
    {
        a3 *= -1;
        b3 *= -1;
        c3 *= -1;
    }

    /* koeficienty pro barycentricke souradnice */
    alpha = 1.0 / ABS(s2);
    beta = 1.0 / ABS(s3);
    gamma = 1.0 / ABS(s1);

    /* vyplnovani... */
    for( y = miny; y <= maxy; ++y )
    {
        /* inicilizace hranove fce v bode (minx, y) */
        e1 = a1 * minx + b1 * y + c1;
        e2 = a2 * minx + b2 * y + c2;
        e3 = a3 * minx + b3 * y + c3;

        for( x = minx; x <= maxx; ++x )
        {
            if( e1 >= 0 && e2 >= 0 && e3 >= 0 )
            {
                /* interpolace pomoci barycentrickych souradnic
                   e1, e2, e3 je aktualni vzdalenost bodu (x, y) od primek */
                w1 = alpha * e2;
                w2 = beta * e3;
                w3 = gamma * e1;

                /* interpolace z-souradnice */
                z = w1 * v1->z + w2 * v2->z + w3 * v3->z;

				textureX = w1 * tCoords[0].x + w2 * tCoords[1].x + w3 * tCoords[2].x;
				textureY = w1 * tCoords[0].y + w2 * tCoords[1].y + w3 * tCoords[2].y;

                /* interpolace barvy */
                color.red = ROUND2BYTE(w1 * col1.red + w2 * col2.red + w3 * col3.red);
                color.green = ROUND2BYTE(w1 * col1.green + w2 * col2.green + w3 * col3.green);
                color.blue = ROUND2BYTE(w1 * col1.blue + w2 * col2.blue + w3 * col3.blue);
                color.alpha = 255;

				textureColor = studrenTextureValue((S_StudentRenderer*)pRenderer, textureX, textureY);
				color.red = color.red * textureColor.red / 255;
				color.green = color.green * textureColor.green / 255;
				color.blue = color.blue * textureColor.blue / 255;


                /* vykresleni bodu */
                if( z < DEPTH(pRenderer, x, y) )
                {
                    PIXEL(pRenderer, x, y) = color;
                    DEPTH(pRenderer, x, y) = z;
                }
            }

            /* hranova fce o pixel vedle */
            e1 += a1;
            e2 += a2;
            e3 += a3;
        }
    }
}

/******************************************************************************
 * Vykresli i-ty trojuhelnik n-teho klicoveho snimku modelu
 * pomoci nove fce studrenDrawTriangle()
 * Pred vykreslenim aplikuje na vrcholy a normaly trojuhelniku
 * aktualne nastavene transformacni matice!
 * Upravte tak, aby se model vykreslil interpolovane dle parametru n
 * (cela cast n udava klicovy snimek, desetinna cast n parametr interpolace
 * mezi snimkem n a n + 1)
 * i - index trojuhelniku
 * n - index klicoveho snimku (float pro pozdejsi interpolaci mezi snimky)
 */

void studrenProjectTriangle(S_Renderer *pRenderer, S_Model *pModel, int i, float n)
{
    /* zaklad fce zkopirujte z render.c */
	S_Coords	a, b, c;
	S_Coords	na, nb, nc;
    S_Coords    aa, bb, cc;             /* souradnice vrcholu po transformaci */
    S_Coords    naa, nbb, ncc;          /* normaly ve vrcholech po transformaci */
    S_Coords    nn;                     /* normala trojuhelniku po transformaci */
	S_Coords	n0;
    int         u1, v1, u2, v2, u3, v3; /* souradnice vrcholu po projekci do roviny obrazovky */
    S_Triangle  * triangle;
    int         vertexOffset, normalOffset; /* offset pro vrcholy a normalove vektory trojuhelniku */
    int         i0, i1, i2, in;             /* indexy vrcholu a normaly pro i-ty trojuhelnik n-teho snimku */

	//Premenne pre n+1
	int         vertexOffsetNext, normalOffsetNext;
	int         i0Next, i1Next, i2Next, inNext;

	float		nInterpolace;	/* Interpolace na zaklade desatinnec casti n */
	float		tmp4;

    IZG_ASSERT(pRenderer && pModel && i >= 0 && i < trivecSize(pModel->triangles) && n >= 0 );

    /* z modelu si vytahneme i-ty trojuhelnik */
    triangle = trivecGetPtr(pModel->triangles, i);

    /* ziskame offset pro vrcholy n-teho snimku */
    vertexOffset = (((int) n) % pModel->frames) * pModel->verticesPerFrame;
	vertexOffsetNext = (((int) (n + 1)) % pModel->frames) * pModel->verticesPerFrame; //n+1

    /* ziskame offset pro normaly trojuhelniku n-teho snimku */
    normalOffset = (((int) n) % pModel->frames) * pModel->triangles->size;
	normalOffsetNext = (((int) (n + 1)) % pModel->frames) * pModel->triangles->size; //n+1

    /* indexy vrcholu pro i-ty trojuhelnik n-teho snimku - pricteni offsetu */
    i0 = triangle->v[ 0 ] + vertexOffset;
    i1 = triangle->v[ 1 ] + vertexOffset;
    i2 = triangle->v[ 2 ] + vertexOffset;
	i0Next = triangle->v[ 0 ] + vertexOffsetNext; //n+1
    i1Next = triangle->v[ 1 ] + vertexOffsetNext; //n+1
    i2Next = triangle->v[ 2 ] + vertexOffsetNext; //n+1

    /* index normaloveho vektoru pro i-ty trojuhelnik n-teho snimku - pricteni offsetu */
    in = triangle->n + normalOffset;
	inNext = triangle->n + normalOffsetNext; //n+1

	/* Interpolacia medzi bodmi */
	a = *cvecGetPtr(pModel->vertices, i0);
	b = *cvecGetPtr(pModel->vertices, i1);
	c = *cvecGetPtr(pModel->vertices, i2);

	/* Interpolacia medzi bodmi n+1 */
	aa = *cvecGetPtr(pModel->vertices, i0Next);
	bb = *cvecGetPtr(pModel->vertices, i1Next);
	cc = *cvecGetPtr(pModel->vertices, i2Next);

	nInterpolace = n - (int)n;
	tmp4 = 1.0 - nInterpolace;

	aa.x = a.x*(tmp4) + aa.x*nInterpolace;	bb.x = b.x*(tmp4) + bb.x*nInterpolace;
	aa.y = a.y*(tmp4) + aa.y*nInterpolace;	bb.y = b.y*(tmp4) + bb.y*nInterpolace;
	aa.z = a.z*(tmp4) + aa.z*nInterpolace;	bb.z = b.z*(tmp4) + bb.z*nInterpolace;

	cc.x = c.x*(tmp4) + cc.x*nInterpolace;
	cc.y = c.y*(tmp4) + cc.y*nInterpolace;
	cc.z = c.z*(tmp4) + cc.z*nInterpolace;

    /* transformace vrcholu matici model */
    trTransformVertex(&a, &aa);
    trTransformVertex(&b, &bb);
    trTransformVertex(&c, &cc);

    /* promitneme vrcholy trojuhelniku na obrazovku */
    trProjectVertex(&u1, &v1, &a);
    trProjectVertex(&u2, &v2, &b);
    trProjectVertex(&u3, &v3, &c);

	na = *cvecGetPtr(pModel->normals, i0);	naa = *cvecGetPtr(pModel->normals, i0Next);
	nb = *cvecGetPtr(pModel->normals, i1);	nbb = *cvecGetPtr(pModel->normals, i1Next);
	nc = *cvecGetPtr(pModel->normals, i2);	ncc = *cvecGetPtr(pModel->normals, i2Next);

	naa.x = na.x*(tmp4) + naa.x*nInterpolace;	nbb.x = nb.x*(tmp4) + nbb.x*nInterpolace;
	naa.y = na.y*(tmp4) + naa.y*nInterpolace;	nbb.y = nb.y*(tmp4) + nbb.y*nInterpolace;
	naa.z = na.z*(tmp4) + naa.z*nInterpolace;	nbb.z = nb.z*(tmp4) + nbb.z*nInterpolace;

	ncc.x = nc.x*(tmp4) + ncc.x*nInterpolace;
	ncc.y = nc.y*(tmp4) + ncc.y*nInterpolace;
	ncc.z = nc.z*(tmp4) + ncc.z*nInterpolace;

    /* pro osvetlovaci model transformujeme take normaly ve vrcholech */
    trTransformVector(&na, &naa);
    trTransformVector(&nb, &nbb);
    trTransformVector(&nc, &ncc);

    /* normalizace normal */
    coordsNormalize(&na);
    coordsNormalize(&nb);
    coordsNormalize(&nc);

	/* transformace normaly trojuhelniku matici model */
	n0 = *cvecGetPtr(pModel->trinormals, in);
	nn = *cvecGetPtr(pModel->trinormals, inNext);

	nn.x = n0.x*(tmp4) + nn.x*nInterpolace;
	nn.y = n0.y*(tmp4) + nn.y*nInterpolace;
	nn.z = n0.z*(tmp4) + nn.z*nInterpolace;

    /* transformace normaly trojuhelniku matici model */
    trTransformVector(&n0, &nn);

    /* normalizace normaly */
    coordsNormalize(&n0);

    /* je troj. privraceny ke kamere, tudiz viditelny? */
    if( !renCalcVisibility(pRenderer, &a, &n0) )
    {
        /* odvracene troj. vubec nekreslime */
        return;
    }

    /* rasterizace trojuhelniku */
    studrenDrawTriangle(pRenderer,
                    &a, &b, &c,
                    &na, &nb, &nc,
                    u1, v1, u2, v2, u3, v3, triangle->t
                    );
}

/******************************************************************************
* Vraci hodnotu v aktualne nastavene texture na zadanych
* texturovacich souradnicich u, v
* Pro urceni hodnoty pouziva bilinearni interpolaci
* Pro otestovani vraci ve vychozim stavu barevnou sachovnici dle uv souradnic
* u, v - texturovaci souradnice v intervalu 0..1, ktery odpovida sirce/vysce textury
*/

S_RGBA studrenTextureValue( S_StudentRenderer * pRenderer, double u, double v )
{
	double		uu, vv;
	double		tmpX, tmpY;
	double		tmp1, tmp2;
	int			w1, w2, w3, w4;
	int 		a, r, g, b;

	uu = v *pRenderer->width;
	vv = u *pRenderer->height;

	const S_RGBA * const pix1 = pRenderer->texture + (int)uu + (int)vv *pRenderer->width;
	const S_RGBA * const pix2 = pix1 + 1;
	const S_RGBA * const pix3 = pix1 + pRenderer->width;
	const S_RGBA * const pix4 = pix3 + 1;

	tmpX = uu - (int)uu;
	tmpY = vv - (int)vv;

	tmp1 = (1.0 - tmpX) * 256;
	tmp2 = 1.0 - tmpY;

	w1 = tmp1 * tmp2;
	w2 = tmpX * tmp2 * 256;
	w3 = tmp1 * tmpY;
	w4 = tmpX  * tmpY  * 256;

	/* vypocitame hodnoty jednotlivych pixelu */
	a = (pix1->alpha * w1 + pix2->alpha * w2 + pix3->alpha * w3 + pix4->alpha * w4) >> 8;
	r = (pix1->red   * w1 + pix2->red   * w2 + pix3->red   * w3 + pix4->red * w4) >> 8;
	g = (pix1->green * w1 + pix2->green * w2 + pix3->green * w3 + pix4->green * w4) >> 8;
	b = (pix1->blue  * w1 + pix2->blue  * w2 + pix3->blue  * w3 + pix4->blue * w4) >> 8;

    return makeColorA(r, g, b, a);
}

/******************************************************************************
 ******************************************************************************
 * Funkce pro vyrenderovani sceny, tj. vykresleni modelu
 * Upravte tak, aby se model vykreslil animovane
 * (volani renderModel s aktualizovanym parametrem n)
 */

void renderStudentScene(S_Renderer *pRenderer, S_Model *pModel)
{
    /* zaklad fce zkopirujte z main.c */

    /* test existence frame bufferu a modelu */
	IZG_ASSERT(pModel && pRenderer);

	/* nastavit projekcni matici */
	trProjectionPerspective(pRenderer->camera_dist, pRenderer->frame_w, pRenderer->frame_h);

	/* vycistit model matici */
	trLoadIdentity();

	/* nejprve nastavime posuv cele sceny od/ke kamere */
	trTranslate(0.0, 0.0, pRenderer->scene_move_z);

	/* nejprve nastavime posuv cele sceny v rovine XY */
	trTranslate(pRenderer->scene_move_x, pRenderer->scene_move_y, 0.0);

	/* natoceni cele sceny - jen ve dvou smerech - mys je jen 2D... :( */
	trRotateX(pRenderer->scene_rot_x);
	trRotateY(pRenderer->scene_rot_y);

	/* nastavime material */
	renMatAmbient(pRenderer, &MAT_WHITE_AMBIENT);
	renMatDiffuse(pRenderer, &MAT_WHITE_DIFFUSE);
	renMatSpecular(pRenderer, &MAT_WHITE_SPECULAR);

	/* a vykreslime nas model (ve vychozim stavu kreslime pouze snimek 0) */
	renderModel(pRenderer, pModel, runTime);
}

/* Callback funkce volana pri tiknuti casovace
 * ticks - pocet milisekund od inicializace */
void onTimer( int ticks )
{
    /* uprava parametru pouzivaneho pro vyber klicoveho snimku
     * a pro interpolaci mezi snimky */
    static int previous_ticks = 0;
	runTime += (float)((ticks - previous_ticks) / 100.0);
	previous_ticks = ticks;

	runTime = (float)(ticks / 100.0);
}

/*****************************************************************************
 *****************************************************************************/
