
# 2020-07-30

## Modell

Log-log Spezifikation sollte am meisten Sinn ergeben - möglicherweise mit on-site Dummy. Distance decay verhält sich ähnlich, lineare explanatories teils auch.
Heteroskedastizität ist vermutlich ein Problem. Für Brasilien finden wir vier Observationen mit hohem Leverage die mehr Aufwand verdienen.

## Ergebnisse

Für folgende Länder gibt es zurzeit erste Ergebnisse: BRA, IDN, MYS, AGO, COL, LAO, VNM, SUR.

#### Log-Linear
Wir finden abnehmenden Forest Loss mit linear steigender Distanz für: BRA, IDN, MYS, AGO, COL, VNM, SUR. Ein Dummy für 5km Umkreis scheint sinnvoll zu sein, aber verändert die Ergebniss nur unbedeutend. Eine Ausnahme ist SUR, wo dann nur dieser relevant ist -- die Abholzung findet im direkten Umfeld statt.
LAO reißt hier aus, und zeigt sinkende Abholzung bei steigender Distanz.

#### Log-log
Ergebnisse bei der logarithmisch steigender Distanz sind etwas gemischter -- wir finden abnehmenden Forest Loss für: BRA, IDN, AGO, COL, SUR. In MYS und VNM ist der Koeffizient nahe bei 0 und nicht sehr robust; bei LAO finden wir steigenden Forest Loss (wie zuvor). Auch hier ist der 5km Dummy relevant, mit tendenziell höherem Einfluss.
Die Ergebnisse sind robust gegenüber dem Auslassen von Population, Land Use, Land Use Gruppen, und Protected Areas - MYS ist hier die Ausnahme.

#### Ausmaß

In der log-log Spezifikation für Brasilien erhalten wir u.a. folgende Koeffizienten:

distance-mine < 5km: 1.21
distance-mine log: -0.3
population log: 0.95
forest-area log: 0.57
distance-road < 5km: -0.52
distance-road log: 0.28
distance-protected-area < 5km: 0.19
distance-protected-area log: 0.18
distance-cropland < 5km: -0.18
distance-cropland log: -1.58

## Länder

Liste der Länder ist weiter gekürzt, u.a. auf Grund von mangelnden Minen-Daten oder mangelndem Wald. Die verbliebenen Länder sind weiterhin nicht komplett einwandfrei:

- Burma wird dominiert von Minen in Indien und China, national gibt es nur eine zentrale Mine.
- Thailand ist im Norden und im Süden sehr bewaldet - nur im Norden gibt es nationale Minen, im Süden dominiert Malaysia.
- Malaysia hat nur eine (zentrale) Mine auf Borneo - die Insel unterscheidet sich stark von der Halbinsel.
- Einige der Länder sind relativ klein, haben wenige Minen oder sehr konzentrierte Cluster.
