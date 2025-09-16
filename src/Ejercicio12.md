# a)
$$
P(\text{e}) \equiv \text{cantLit} \ \text{e} \ = \ \text{S} \ (\text{cantOp} \ \text{e})
$$
# b)
```
Para probar la propiedad P(e) ∀ e :: Expr basta con verificar por inducción estructural que P(e) es verdadera en los siguientes casos:

∀ a :: Float.  ∀ b :: Float.
Casos base:
    1. Si e = Const a   , vale  P(Const a)
    2. Si e = Rango a b , vale  P(Rango a b)

∀ e1 :: Expr. ∀ e2 :: Expr.
Caso inductivo:
    1. Si e = Suma e1 e2 , vale P(Suma e1 e2) segun
        Hipótesis inductiva:  P(e1) ∧ P(e2) => P(Suma e1 e2)

    2. Si e = Resta e1 e2 , vale P(Resta e1 e2) segun
        Hipótesis inductiva:  P(e1) ∧ P(e2) => P(Resta e1 e2)

    3. Si e = Mult e1 e2 , vale P(Mult e1 e2) segun
        Hipótesis inductiva:  P(e1) ∧ P(e2) => P(Mult e1 e2)

    4. Si e = Div e1 e2 , vale P(Div e1 e2) segun
        Hipótesis inductiva:  P(e1) ∧ P(e2) => P(Div e1 e2)

Verificados los casos base y los casos inductivos entonces se puede concluir que por principio de inducción estructural que ∀ e :: Expr. P(e)
```
# c)

<h2 align="center"> Casos base </h2>

### e = Const a. Con a : : Float


$$
\begin{align*}
P(\text{Const a}) &\equiv \\
\quad\text{cantLit} \ (\text{Const a}) &= S\ (\text{cantOp} \ (\text{Const a})) \\
\quad\text{cantLit} \ (\text{Const a}) &= S \ Z \quad & \text{ \{O1\}} \\
\quad S \ Z &= S \ Z  \quad & \text{ \{L1\}} \\
\end{align*}
$$
El caso base $P\ (\text{Const a})$ queda verificado.

---

### e = Rango a b. Con a, b : : Float

$$
\begin{align*}
P(\text{Rango a b}) &\equiv \\
\quad\text{cantLit} \ (\text{Rango a b}) &= S\ (\text{cantOp} \ (\text{Rango a b})) \\
\quad\text{cantLit} \ (\text{Rango a b}) &= S \ Z \quad & \text{ \{O2\}} \\
\quad S \ Z &= S \ Z  \quad & \text{ \{L2\}} \\
\end{align*}
$$
El caso base $P\ (\text{Rango a b})$ queda verificado.

<p align="center"> <span style="font-size: 20px"> .................................. </spans></p>
<p align="center"> <span style="font-size: 20px"> Los casos bases se ven verificados </spans></p>
<p align="center"> <span style="font-size: 20px"> .................................. </spans></p>
<br></br>

---

<h2 align="center"> Caso inductivo: contructor Suma </h2>

### e = Suma e1 e2

$$
\begin{align*}
&\forall\ e_1 : \ : \text{Expr.} \ \ \forall\ e_2 : \ : \text{Expr.} \\
\end{align*}
$$
$$
\text{Sean } \\ . \\ e\ =\ \text{Suma } e_1 \ e_2 \\ .\\ HI \equiv P\ (e_1) \land P(e_2) \\ .\\ \text{Queremos ver que } P\ (e_1) \land P(e_2) \implies P\ (e)
$$
#### Dem
$$
\begin{align*}
\quad\text{cantLit} \ (\text{Suma} \ e_1 \ e_2) &= S\ (\text{cantOp} \ (\text{Suma} \ e_1 \ e_2)) \\
\quad\text{suma} \ (\text{cantLit} \ e_1)\ (\text{cantLit}\ e_2) &= S\ (\text{cantOp} \ (\text{Suma} \ e_1 \ e_2)) & \text{\{L3\}}\\
\quad\text{suma} \ (S\ (\text{cantOp} \ e_1))\ (S\ (\text{cantOp}\ e_2)) &= S\ (\text{cantOp} \ (\text{Suma} \ e_1 \ e_2)) & \text{\{HI\}}\\
\quad S\ (\text{suma} \ (\text{cantOp} \ e_1)\ (S\ (\text{cantOp}\ e_2))) &= S\ (\text{cantOp} \ (\text{Suma} \ e_1 \ e_2)) & \text{\{S2\}}\\
\quad S\ (\text{suma} \ (S\ (\text{cantOp}\ e_2))\ (\text{cantOp} \ e_1)) &= S\ (\text{cantOp} \ (\text{Suma} \ e_1 \ e_2)) & \text{\{CONMUT\}}\\
\quad S\ (S\ (\text{suma} \ (\text{cantOp}\ e_2)\ (\text{cantOp} \ e_1))) &= S\ (\text{cantOp} \ (\text{Suma} \ e_1 \ e_2)) & \text{\{S2\}}\\
\quad S\ (S\ (\text{suma} \ (\text{cantOp} \ e_1)\ (\text{cantOp}\ e_2))) &= S\ (\text{cantOp} \ (\text{Suma} \ e_1 \ e_2)) & \text{\{CONMUT\}}\\
\quad S\ (S\ (\text{suma} \ (\text{cantOp} \ e_1)\ (\text{cantOp}\ e_2))) &= S\ (S\ (\text{suma} \ (\text{cantOp} \ e_1) \ (\text{cantOp} \ e_2))) & \text{\{O3\}}\\
\end{align*}
$$

El caso inductivo $P\ (\text{Suma}\ e_1\ e_2 )$ queda verificado.

<p align="center"> <span style="font-size: 20px"> ................................................................................................ </spans></p>
<p align="center"> <span style="font-size: 20px"> Por consigna podemos dejar la demostración hasta este punto porque los demás casos son análogos. </span></p>
<p align="center"> <span style="font-size: 20px"> ................................................................................................ </spans></p>
<br></br>

---

<h2 align="center"> Conclusión </h2>

<p align="center"> <span style="font-size: 15px"> Como los casos base y los casos inductivos quedan verificados, entonces por principio de inducción estructural en estructura Expr podemos concluir que </span> </p>

$$
\forall \text{e} \ : \ : \text{Expr}. \ P\ (\text{e}) \ \equiv \ \text{cantLit} \ \text{e} \ = \ \text{S} \ (\text{cantOp} \ \text{e})
$$

<p align="center"> <span style="font-size: 15px"> Es verdadero. </span> </p>
