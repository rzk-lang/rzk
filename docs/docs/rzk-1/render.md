# Rendering Diagrams

Starting from version `0.3.0`, `rzk` supports rendering of topes, types, and terms as diagrams.

This is a literate `rzk` file:

```rzk
#lang rzk-1
```

To enable rendering, enable option `"render" = "svg"` (to disable, `"render" = "none"`):

```rzk
#set-option "render" = "svg"  -- enable rendering in SVG
```

Rendering is completely automatic, and works in the following situations:

1. Mapping from a shape (including curried mappings), up to 3 dimensions, only in products of `2` cubes;
2. Type of mapping from a shape (including curried mappings), up to 3 dimensions, only in products of `2` cubes.
3. Mappings from a shape that is a section of an existing shape.

The rendering assigns the following colors:

- purple is assigned for parameters (context) variables;
- blue is used for fillings for types (e.g. for `hom` and `hom2`);
- red is used for terms (e.g. `Segal-comp-witness`);
- orange is used for shapes in the tope layer;
- grey is used for discarded parts of a (larger) mapping (e.g. when extracting a diagonal/face from a larger shape).

The SVG pictures can be inserted directly into `.md` files before a corresponding `rzk` code block. At the bottom of a markdown file, you might want to add stylization, e.g.:

```html
<style>
.zoom { transition: transform .2s; /* Animation */ }
.zoom:hover { transform: scale(2); /* (200% zoom - Note: if the zoom is too large, it will go outside of the viewport) */ }
</style>

<!-- Definitions for the SVG images above -->
<svg width="0" height="0">
  <defs>
    <style data-bx-fonts="Noto Serif">@import url(https://fonts.googleapis.com/css2?family=Noto+Serif&display=swap);</style>
    <marker id="arrow" viewBox="0 0 10 10" refX="5" refY="5"
      markerWidth="5" markerHeight="5" orient="auto-start-reverse">
      <path d="M 0 2 L 5 5 L 0 8 z" stroke="purple" fill="purple" />
    </marker>
  </defs>
  <style>
    text, textPath {
      font-family: Noto Serif;
      font-size: 28px;
      dominant-baseline: middle;
      text-anchor: middle;
    }
  </style>
</svg>
```

## Examples

### Visualising Simplicial Topes

Topes are visualised with <span style="color: orange">**orange**</span> color:

<svg class="zoom" style="float: right" viewBox="-175 -200 350 375" width="150" height="150">
  <path d="M -88.57160646618001 -39.70915201762215 L 98.4961027394271 -39.70915201762215 L 89.32046645922577 100.78557152253414 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="33.08165424415762" y="7.122422495763279" fill="orange"></text>
  <polyline points="-90.03982894447489,-47.97354751998429 90.03982894447466,-47.97354751998429" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-1.1368683772161603e-13" y="-47.97354751998429" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-94.34447446980596,-35.57774791227483 83.54960825780415,104.91856291954896" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-5.3974331060009035" y="34.670407503637065" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="108.73641627786131,-28.016064827507464 100.54837539908644,97.35687983478158" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="104.64239583847387" y="34.67040750363706" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <text x="-110.03982894447489" y="-47.97354751998429" fill="orange">•</text>
  <text x="110.03982894447466" y="-47.97354751998429" fill="orange">•</text>
  <text x="99.24496273247308" y="117.31436252725841" fill="orange">•</text>
</svg>

```rzk
-- 2-simplex
#def Δ² : (2 * 2) -> TOPE
  := \(t, s) -> s <= t
```
<br><br>
Boundary of a tope:

<svg class="zoom" style="float: right" viewBox="-175 -200 350 375" width="150" height="150">
  <polyline points="-90.03982894447489,-47.97354751998429 90.03982894447466,-47.97354751998429" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-1.1368683772161603e-13" y="-47.97354751998429" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-94.34447446980596,-35.57774791227483 83.54960825780415,104.91856291954896" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-5.3974331060009035" y="34.670407503637065" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="108.73641627786131,-28.016064827507464 100.54837539908644,97.35687983478158" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="104.64239583847387" y="34.67040750363706" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <text x="-110.03982894447489" y="-47.97354751998429" fill="orange">•</text>
  <text x="110.03982894447466" y="-47.97354751998429" fill="orange">•</text>
  <text x="99.24496273247308" y="117.31436252725841" fill="orange">•</text>
</svg>

```rzk
-- boundary of a 2-simplex
#def ∂Δ² : Δ² -> TOPE
  := \(t, s) -> s === 0_2 \/ t === 1_2 \/ s === t
```

The busiest tope diagram involves the entire 3D cube:
<br><br>

<svg class="zoom" style="float: right" viewBox="-175 -200 350 375" width="150" height="150">
  <path d="M -52.83267088231261 -152.25055542320985 L -135.70842732224426 -75.65955496366993 L -124.339192801567 62.47582942420691 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="-104.29343033537462" y="-55.14476032089095" fill="orange"></text>
  <path d="M -43.80286734010653 -157.76177111285438 L -126.67862378003815 -81.17077065331445 L 38.19727095814243 -36.72605298939448 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="-44.094740054000745" y="-91.88619825185442" fill="orange"></text>
  <path d="M -44.06743589162086 -149.41152168372582 L -126.9431923315525 -72.82052122418592 L 33.43503703088448 113.57843673491905 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="-45.858530397429625" y="-36.217868724330906" fill="orange"></text>
  <path d="M -47.77224779767111 -149.10239719544043 L -44.62081179869731 -18.992706863820583 L -119.2787697169255 65.62398765197631 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="-70.55727643776464" y="-34.15703880242824" fill="orange"></text>
  <path d="M -35.351480284498585 -151.82038404642165 L -32.200044285524775 -21.71069371480182 L 104.29504552018004 16.700224334313994 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="12.247840316718893" y="-52.276951142303155" fill="orange"></text>
  <path d="M -39.00701280697936 -146.2633634559564 L -35.85557680800556 -16.153673124336574 L 38.49546011552598 116.72659496268847 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="-12.12237649981965" y="-15.23014720586817" fill="orange"></text>
  <path d="M -43.39865739040455 -141.28591083738013 L -114.90517930965893 73.44047401003661 L 34.10381553210079 121.70404758126475 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="-41.400007055987565" y="17.952870251307075" fill="orange"></text>
  <path d="M -30.122639685828126 -160.23416546907617 L 119.56547399697303 -125.673869065307 L 51.87749861242084 -39.19844734561629 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="47.10677764118858" y="-108.36882729333315" fill="orange"></text>
  <path d="M -26.73167571486168 -157.4409366304129 L 122.95643796793948 -122.88064022664372 L 112.91485008981695 11.079671750322774 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="69.71320411429825" y="-89.74730170224461" fill="orange"></text>
  <path d="M -30.387208237342456 -151.88391603994765 L 119.30090544545868 -117.32361963617848 L 47.115264685162884 111.10604237869723 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="45.3429872977597" y="-52.70049776580962" fill="orange"></text>
  <path d="M -34.36885384819847 -146.79712652702466 L 47.631284450050494 -25.761408403564776 L 43.13361907430687 116.19283189162023 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="18.7986832253863" y="-18.788567679656406" fill="orange"></text>
  <path d="M -30.977889877232023 -144.00389768836138 L 108.66863592744662 24.516710692374282 L 46.524583045273324 118.98606073028348 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="41.40510969849597" y="-0.16704208856786806" fill="orange"></text>
  <path d="M -131.14945832680274 -60.189557409631995 L -119.7802238061255 77.94582697824485 L 29.22877103563421 126.20940054947297 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="-73.900303699098" y="47.98855670602861" fill="orange"></text>
  <path d="M -122.11965478459666 -65.70077309927652 L 42.75623995358392 -21.25605543535655 L 38.2585745778403 120.69818485982844 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="-13.701613417724145" y="11.247118775065124" fill="orange"></text>
  <path d="M -40.06184280325582 -3.5227093097826554 L -114.719800721484 81.09398520601425 L 34.289194120275724 129.3575587772424 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="-40.16414980148803" y="68.97627822449134" fill="orange"></text>
  <path d="M -27.641075290083286 -6.240696160763889 L 108.85401451562154 32.17022188835192 L 46.70996163344825 126.63957192626114 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="42.640966952995505" y="50.85636588461639" fill="orange"></text>
  <path d="M 124.12444299241452 -110.20387151126906 L 56.436467607862326 -23.728449791578353 L 51.938802232118704 118.22579050360666 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="77.49990427746518" y="-5.235510266413584" fill="orange"></text>
  <path d="M 127.51540696338097 -107.41064267260579 L 117.47381908525844 26.549669304360705 L 55.32976620308516 121.01901934226993 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="100.10633075057486" y="13.386015324674949" fill="orange"></text>
  <polyline points="-58.43945016784754,-155.81263120710238 -126.56416054267538,-92.85405384067701" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-92.50180535526147" y="-124.3333425238897" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-43.267074867680414,-149.39273637737728 -40.528074150012564,-36.310784115013945" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-41.89757450884649" y="-92.85176024619561" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-50.07043243065267,-150.41137936513724 -121.55760825554407,64.25691124427178" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-85.81402034309838" y="-43.07723406043273" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-24.26401823142078,-164.88759497832305 112.86496060646587,-133.2269771938925" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="44.30047118752255" y="-149.05728608610778" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-32.5336285617643,-152.82901930722872 41.50165871968916,-43.54976260652718" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="4.484015078962429" y="-98.18939095687796" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-30.99018144517942,-153.98712182088258 107.7774910224333,13.472916680392292" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="38.39365478862694" y="-70.25710257024514" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-38.0977970126922,-150.20257599443408 41.77445614033044,120.82878266324879" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="1.8383295638191228" y="-14.686896665592649" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-139.6116973412595,-59.347211759053934 -129.5172332742686,63.29980300235307" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-134.56446530776404" y="1.9762956216495695" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-121.94155476392162,-74.07433139017691 33.40869499251513,-32.19739115941439" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-44.266429885703246" y="-53.135861274795644" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-128.20795281727402,-64.11913005169937 34.383722015580915,124.85239608467866" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-46.91211540084655" y="30.366633016489644" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-53.27583956202708,-1.3195542994022489 -114.64462936067106,68.23531009808956" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-83.96023446134907" y="33.45787789934366" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-20.79156680420789,-10.898903840409918 101.28644814496036,23.454922619472406" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="40.247440670376236" y="6.278009389531244" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-30.27791262849616,1.1369377628701294 37.662143519633,122.55949282549733" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="3.692115445568419" y="61.84821529418373" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-108.84983961892344,89.39514161516004 28.40117884155652,133.85034134473318" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-40.22433038868346" y="111.62274147994661" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="120.02483210658693,-112.97862167098565 65.0468612075746,-42.74098800304182" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="92.53584665708075" y="-77.85980483701374" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="130.85730851717483,-108.78365343107289 122.0336642163157,8.928620530311047" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="126.44548636674526" y="-49.92751645038092" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="126.32588298339418,-109.65724468567551 53.454439300480715,120.94262359421863" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="89.89016114193745" y="5.6426894542715615" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="52.08603017426715,-7.001940857474867 48.06137989248752,120.02311002447766" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="50.07370503337734" y="56.5105845835014" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="109.5471837166916,45.58156379718138 58.419505769392075,123.30418214308702" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="83.98334474304184" y="84.4428729701342" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <text x="-43.751360390595785" y="-169.386872205972" fill="orange">•</text>
  <text x="-141.25225031992713" y="-79.2798128418074" fill="orange">•</text>
  <text x="-40.04378862709719" y="-16.316648286419234" fill="orange">•</text>
  <text x="-127.87668029560095" y="83.23240408510654" fill="orange">•</text>
  <text x="132.35230276564087" y="-128.72769996624356" fill="orange">•</text>
  <text x="52.71939054852064" y="-26.9919097077839" fill="orange">•</text>
  <text x="120.53866996784966" y="28.87266706548172" fill="orange">•</text>
  <text x="47.42801951823403" y="140.0130788747867" fill="orange">•</text>
</svg>

```rzk
-- 3-dim cube
#def 2³ : (2 * 2 * 2) -> TOPE
  := \_ -> TOP
```
<br><br><br>
<svg class="zoom" style="float: right" viewBox="-175 -200 350 375" width="150" height="150">
  <path d="M -26.73167571486168 -157.4409366304129 L 122.95643796793948 -122.88064022664372 L 112.91485008981695 11.079671750322774 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="69.71320411429825" y="-89.74730170224461" fill="orange"></text>
  <path d="M -30.387208237342456 -151.88391603994765 L 119.30090544545868 -117.32361963617848 L 47.115264685162884 111.10604237869723 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="45.3429872977597" y="-52.70049776580962" fill="orange"></text>
  <path d="M -30.977889877232023 -144.00389768836138 L 108.66863592744662 24.516710692374282 L 46.524583045273324 118.98606073028348 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="41.40510969849597" y="-0.16704208856786806" fill="orange"></text>
  <path d="M 127.51540696338097 -107.41064267260579 L 117.47381908525844 26.549669304360705 L 55.32976620308516 121.01901934226993 Z" style="fill: orange; opacity: 0.2"><title></title></path>
  <text x="100.10633075057486" y="13.386015324674949" fill="orange"></text>
  <polyline points="-24.26401823142078,-164.88759497832305 112.86496060646587,-133.2269771938925" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="44.30047118752255" y="-149.05728608610778" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-30.99018144517942,-153.98712182088258 107.7774910224333,13.472916680392292" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="38.39365478862694" y="-70.25710257024514" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-38.0977970126922,-150.20257599443408 41.77445614033044,120.82878266324879" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="1.8383295638191228" y="-14.686896665592649" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="130.85730851717483,-108.78365343107289 122.0336642163157,8.928620530311047" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="126.44548636674526" y="-49.92751645038092" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="126.32588298339418,-109.65724468567551 53.454439300480715,120.94262359421863" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="89.89016114193745" y="5.6426894542715615" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="109.5471837166916,45.58156379718138 58.419505769392075,123.30418214308702" stroke="orange" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="83.98334474304184" y="84.4428729701342" fill="orange" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <text x="-43.751360390595785" y="-169.386872205972" fill="orange">•</text>
  <text x="132.35230276564087" y="-128.72769996624356" fill="orange">•</text>
  <text x="120.53866996784966" y="28.87266706548172" fill="orange">•</text>
  <text x="47.42801951823403" y="140.0130788747867" fill="orange">•</text>
</svg>

```rzk
-- 3-simplex
#def Δ³ : (2 * 2 * 2) -> TOPE
  := \((t1, t2), t3) -> t3 <= t2 /\ t2 <= t1
```

<br><br>
### Visualising Simplicial Types

Types are visualised with <span style="color: blue">**blue**</span> color. Recognised parameter part (e.g. fixed endpoints, edges, faces with clear labels) are visualised with <span style="color: purple">**purple**</span> color. When a type is constructed by taking a part of another shape, the rest of the larger shape is colored using <span style="color: gray">**gray**</span> color.

<svg class="zoom" style="float: right" viewBox="-175 -200 350 375" width="150" height="150">
  <polyline points="-79.24496273247331,117.31436252725841 79.24496273247308,117.31436252725841" stroke="blue" stroke-width="3" marker-end="url(#arrow)"><title></title></polyline>
  <text x="-1.1368683772161603e-13" y="117.31436252725841" fill="blue" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <text x="-99.24496273247331" y="117.31436252725841" fill="purple">x</text>
  <text x="99.24496273247308" y="117.31436252725841" fill="purple">y</text>
</svg>

```rzk
-- [RS17, Definition 5.1]
-- The type of arrows in A from x to y.
#def hom
  (A : U)   -- A type.
  (x y : A) -- Two points in A.
  : U                   -- (hom A x y) is a 1-simplex (an arrow)
  := (t : 2) -> A [    -- in A where
    t === 0_2 |-> x,    -- * the left endpoint is exactly x
    t === 1_2 |-> y     -- * the right endpoint is exactly y
  ]
```

<svg class="zoom" style="float: right" viewBox="-175 -200 350 375" width="150" height="150">
  <path d="M -88.57160646618001 -39.70915201762215 L 98.4961027394271 -39.70915201762215 L 89.32046645922577 100.78557152253414 Z" style="fill: blue; opacity: 0.2"><title></title></path>
  <text x="33.08165424415762" y="7.122422495763279" fill="blue"></text>
  <polyline points="-90.03982894447489,-47.97354751998429 90.03982894447466,-47.97354751998429" stroke="purple" stroke-width="3" marker-end="url(#arrow)"><title>f</title></polyline>
  <text x="-1.1368683772161603e-13" y="-47.97354751998429" fill="purple" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke">f</text>
  <polyline points="-94.34447446980596,-35.57774791227483 83.54960825780415,104.91856291954896" stroke="purple" stroke-width="3" marker-end="url(#arrow)"><title>h</title></polyline>
  <text x="-5.3974331060009035" y="34.670407503637065" fill="purple" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke">h</text>
  <polyline points="108.73641627786131,-28.016064827507464 100.54837539908644,97.35687983478158" stroke="purple" stroke-width="3" marker-end="url(#arrow)"><title>g</title></polyline>
  <text x="104.64239583847387" y="34.67040750363706" fill="purple" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke">g</text>
  <text x="-110.03982894447489" y="-47.97354751998429" fill="purple">x</text>
  <text x="110.03982894447466" y="-47.97354751998429" fill="purple">y</text>
  <text x="99.24496273247308" y="117.31436252725841" fill="purple">z</text>
</svg>

```rzk
-- [RS17, Definition 5.2]
-- the type of commutative triangles in A
#def hom2
  (A : U)           -- A type.
  (x y z : A)       -- Three points in A.
  (f : hom A x y)   -- An arrow in A from x to y.
  (g : hom A y z)   -- An arrow in A from y to z.
  (h : hom A x z)   -- An arrow in A from x to z.
  : U                           -- (hom2 A x y z f g h) is a 2-simplex (triangle)
  := { (t1, t2) : Δ² } -> A [   -- in A where
    t2 === 0_2 |-> f t1,        -- * the top edge is exactly f,
    t1 === 1_2 |-> g t2,        -- * the right edge is exactly g, and
    t2 === t1  |-> h t2         -- * the diagonal is exactly h
  ]
```

### Visualising Terms of Simplicial Types

Terms (with non-trivial labels) are visualised with <span style="color: red">**red**</span> color (you can see a detailed label on hover). Recognised parameter part (e.g. fixed endpoints, edges, faces with clear labels) are visualised with <span style="color: purple">**purple**</span> color. When a term is constructed by taking a part of another shape, the rest of the larger shape is colored using <span style="color: gray">**gray**</span> color.

We can visualise terms that fill a shape:

<svg class="zoom" style="float: right" viewBox="-175 -200 350 375" width="150" height="150">
  <path d="M -99.0358460500274 -31.444756515260025 L -89.86020976982607 109.04996702489628 L 78.85622687537837 109.04996702489628 Z" style="fill: red; opacity: 0.2"><title>second a (second x₂, first x₂)</title></path>
  <text x="-36.6799429814917" y="62.21839251151084" fill="red"></text>
  <path d="M -88.57160646618001 -39.70915201762215 L 98.4961027394271 -39.70915201762215 L 89.32046645922577 100.78557152253414 Z" style="fill: red; opacity: 0.2"><title>second a (first x₂, second x₂)</title></path>
  <text x="33.08165424415762" y="7.122422495763279" fill="red"></text>
  <polyline points="-108.73641627786154,-28.016064827507464 -100.54837539908667,97.35687983478158" stroke="purple" stroke-width="3" marker-end="url(#arrow)"><title>f</title></polyline>
  <text x="-104.6423958384741" y="34.67040750363706" fill="purple" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke">f</text>
  <polyline points="-90.03982894447489,-47.97354751998429 90.03982894447466,-47.97354751998429" stroke="purple" stroke-width="3" marker-end="url(#arrow)"><title>f</title></polyline>
  <text x="-1.1368683772161603e-13" y="-47.97354751998429" fill="purple" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke">f</text>
  <polyline points="-94.34447446980596,-35.57774791227483 83.54960825780415,104.91856291954896" stroke="red" stroke-width="3" marker-end="url(#arrow)"><title>first a (second (first x₂, second x₂))</title></polyline>
  <text x="-5.3974331060009035" y="34.670407503637065" fill="red" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-79.24496273247331,117.31436252725841 79.24496273247308,117.31436252725841" stroke="purple" stroke-width="3" marker-end="url(#arrow)"><title>g</title></polyline>
  <text x="-1.1368683772161603e-13" y="117.31436252725841" fill="purple" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke">g</text>
  <polyline points="108.73641627786131,-28.016064827507464 100.54837539908644,97.35687983478158" stroke="purple" stroke-width="3" marker-end="url(#arrow)"><title>g</title></polyline>
  <text x="104.64239583847387" y="34.67040750363706" fill="purple" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke">g</text>
  <text x="-110.03982894447489" y="-47.97354751998429" fill="purple">x</text>
  <text x="-99.24496273247331" y="117.31436252725841" fill="purple">y</text>
  <text x="110.03982894447466" y="-47.97354751998429" fill="purple">y</text>
  <text x="99.24496273247308" y="117.31436252725841" fill="purple">z</text>
</svg>

```rzk
#def square
  (A : U)
  (x y z : A)
  (f : hom A x y)
  (g : hom A y z)
  (h : hom A x z)
  (a : Sigma (h' : hom A x z), hom2 A x y z f g h')
  : (2 * 2) -> A
  := \(t, s) -> recOR( s <= t |-> second a (t, s) , t <= s |-> second a (s, t))
```

If a term is extracted as a part of a larger shape, generally, the whole shape will be shown (in gray):

<svg class="zoom" style="float: right" viewBox="-175 -200 350 375" width="150" height="150">
  <path d="M -35.351480284498585 -151.82038404642165 L -32.200044285524775 -21.71069371480182 L 104.29504552018004 16.700224334313994 Z" style="fill: gray; opacity: 0.2"><title>second a x₂</title></path>
  <text x="12.247840316718893" y="-52.276951142303155" fill="gray"></text>
  <path d="M -39.00701280697936 -146.2633634559564 L -35.85557680800556 -16.153673124336574 L 38.49546011552598 116.72659496268847 Z" style="fill: gray; opacity: 0.2"><title>second a x₂</title></path>
  <text x="-12.12237649981965" y="-15.23014720586817" fill="gray"></text>
  <path d="M -30.122639685828126 -160.23416546907617 L 119.56547399697303 -125.673869065307 L 51.87749861242084 -39.19844734561629 Z" style="fill: gray; opacity: 0.2"><title>second a x₂</title></path>
  <text x="47.10677764118858" y="-108.36882729333315" fill="gray"></text>
  <path d="M -26.73167571486168 -157.4409366304129 L 122.95643796793948 -122.88064022664372 L 112.91485008981695 11.079671750322774 Z" style="fill: gray; opacity: 0.2"><title>second a x₂</title></path>
  <text x="69.71320411429825" y="-89.74730170224461" fill="gray"></text>
  <path d="M -30.387208237342456 -151.88391603994765 L 119.30090544545868 -117.32361963617848 L 47.115264685162884 111.10604237869723 Z" style="fill: gray; opacity: 0.2"><title>second a x₂</title></path>
  <text x="45.3429872977597" y="-52.70049776580962" fill="gray"></text>
  <path d="M -34.36885384819847 -146.79712652702466 L 47.631284450050494 -25.761408403564776 L 43.13361907430687 116.19283189162023 Z" style="fill: gray; opacity: 0.2"><title>second a x₂</title></path>
  <text x="18.7986832253863" y="-18.788567679656406" fill="gray"></text>
  <path d="M -30.977889877232023 -144.00389768836138 L 108.66863592744662 24.516710692374282 L 46.524583045273324 118.98606073028348 Z" style="fill: red; opacity: 0.2"><title>second a x₂</title></path>
  <text x="41.40510969849597" y="-0.16704208856786806" fill="red"></text>
  <path d="M -27.641075290083286 -6.240696160763889 L 108.85401451562154 32.17022188835192 L 46.70996163344825 126.63957192626114 Z" style="fill: gray; opacity: 0.2"><title>second a x₂</title></path>
  <text x="42.640966952995505" y="50.85636588461639" fill="gray"></text>
  <path d="M 124.12444299241452 -110.20387151126906 L 56.436467607862326 -23.728449791578353 L 51.938802232118704 118.22579050360666 Z" style="fill: gray; opacity: 0.2"><title>first a (second x₂)</title></path>
  <text x="77.49990427746518" y="-5.235510266413584" fill="gray"></text>
  <path d="M 127.51540696338097 -107.41064267260579 L 117.47381908525844 26.549669304360705 L 55.32976620308516 121.01901934226993 Z" style="fill: gray; opacity: 0.2"><title>first a (second x₂)</title></path>
  <text x="100.10633075057486" y="13.386015324674949" fill="gray"></text>
  <polyline points="-43.267074867680414,-149.39273637737728 -40.528074150012564,-36.310784115013945" stroke="gray" stroke-width="3" marker-end="url(#arrow)"><title>f</title></polyline>
  <text x="-41.89757450884649" y="-92.85176024619561" fill="gray" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke">f</text>
  <polyline points="-24.26401823142078,-164.88759497832305 112.86496060646587,-133.2269771938925" stroke="gray" stroke-width="3" marker-end="url(#arrow)"><title>second a x₂</title></polyline>
  <text x="44.30047118752255" y="-149.05728608610778" fill="gray" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-32.5336285617643,-152.82901930722872 41.50165871968916,-43.54976260652718" stroke="gray" stroke-width="3" marker-end="url(#arrow)"><title>second a x₂</title></polyline>
  <text x="4.484015078962429" y="-98.18939095687796" fill="gray" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-30.99018144517942,-153.98712182088258 107.7774910224333,13.472916680392292" stroke="red" stroke-width="3" marker-end="url(#arrow)"><title>second a x₂</title></polyline>
  <text x="38.39365478862694" y="-70.25710257024514" fill="red" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-38.0977970126922,-150.20257599443408 41.77445614033044,120.82878266324879" stroke="red" stroke-width="3" marker-end="url(#arrow)"><title>second a x₂</title></polyline>
  <text x="1.8383295638191228" y="-14.686896665592649" fill="red" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-20.79156680420789,-10.898903840409918 101.28644814496036,23.454922619472406" stroke="gray" stroke-width="3" marker-end="url(#arrow)"><title>second a x₂</title></polyline>
  <text x="40.247440670376236" y="6.278009389531244" fill="gray" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="-30.27791262849616,1.1369377628701294 37.662143519633,122.55949282549733" stroke="gray" stroke-width="3" marker-end="url(#arrow)"><title>second a x₂</title></polyline>
  <text x="3.692115445568419" y="61.84821529418373" fill="gray" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="120.02483210658693,-112.97862167098565 65.0468612075746,-42.74098800304182" stroke="gray" stroke-width="3" marker-end="url(#arrow)"><title>first a (second x₂)</title></polyline>
  <text x="92.53584665708075" y="-77.85980483701374" fill="gray" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="130.85730851717483,-108.78365343107289 122.0336642163157,8.928620530311047" stroke="gray" stroke-width="3" marker-end="url(#arrow)"><title>y</title></polyline>
  <text x="126.44548636674526" y="-49.92751645038092" fill="gray" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke">y</text>
  <polyline points="126.32588298339418,-109.65724468567551 53.454439300480715,120.94262359421863" stroke="gray" stroke-width="3" marker-end="url(#arrow)"><title>first a (second x₂)</title></polyline>
  <text x="89.89016114193745" y="5.6426894542715615" fill="gray" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <polyline points="52.08603017426715,-7.001940857474867 48.06137989248752,120.02311002447766" stroke="gray" stroke-width="3" marker-end="url(#arrow)"><title>z</title></polyline>
  <text x="50.07370503337734" y="56.5105845835014" fill="gray" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke">z</text>
  <polyline points="109.5471837166916,45.58156379718138 58.419505769392075,123.30418214308702" stroke="red" stroke-width="3" marker-end="url(#arrow)"><title>first a (second x₂)</title></polyline>
  <text x="83.98334474304184" y="84.4428729701342" fill="red" stroke="white" stroke-width="10" stroke-opacity=".8" paint-order="stroke"></text>
  <text x="-43.751360390595785" y="-169.386872205972" fill="purple">x</text>
  <text x="-40.04378862709719" y="-16.316648286419234" fill="gray">y</text>
  <text x="132.35230276564087" y="-128.72769996624356" fill="gray">y</text>
  <text x="52.71939054852064" y="-26.9919097077839" fill="gray">z</text>
  <text x="120.53866996784966" y="28.87266706548172" fill="purple">y</text>
  <text x="47.42801951823403" y="140.0130788747867" fill="purple">z</text>
</svg>

```rzk
#def face
  (A : U)
  (x y z : A)
  (f : hom A x y)
  (a : Sigma (g : hom A y z), {((t1, t2), t3) : 2 * 2 * 2 | t3 <= t1 \/ t2 <= t1} -> A [ t1 === 0_2 |-> f t2, t1 === 1_2 |-> g t3 ])
  : Δ² -> A
  := \(t, s) -> second a ((t, t), s)
```

<!-- Style for the SVG images above -->
<style>
.zoom { transition: transform .2s; /* Animation */ }
.zoom:hover { transform: scale(2); /* (200% zoom - Note: if the zoom is too large, it will go outside of the viewport) */ }
</style>

<!-- Definitions for the SVG images above -->
<svg width="0" height="0">
  <defs>
    <style data-bx-fonts="Noto Serif">@import url(https://fonts.googleapis.com/css2?family=Noto+Serif&display=swap);</style>
    <marker id="arrow" viewBox="0 0 10 10" refX="5" refY="5"
      markerWidth="5" markerHeight="5" orient="auto-start-reverse">
      <path d="M 0 2 L 5 5 L 0 8 z" stroke="black" fill="black" />
    </marker>
  </defs>
  <style>
    text, textPath {
      font-family: Noto Serif;
      font-size: 28px;
      dominant-baseline: middle;
      text-anchor: middle;
    }
  </style>
</svg>