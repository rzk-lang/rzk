# Решатель теорем Rzk

Rzk — это экспериментальный интерактивный решатель теорем,
основанный на [«Теории типов для синтетических ∞-категорий](https://arxiv.org/abs/1705.07442)[^1].

[Первые шаги с Rzk](getting-started/install.md){ .md-button .md-button--primary }
[Попробовать Rzk в онлайн-песочнице](playground/index.html){ .md-button }

## Об этом проекте

Проект начался с идеи воплотить "в жизнь" статью Эмили Рил и Майкла Шульмана 2017 года[^1],
реализуя решатель для их теории типов с формами (type theory with shapes).
На момент написания этого текста, Rzk поддерживает формальный язык, близкий к теории типов в упомянутой статье,
а также вокруг решателя существует некая инструментальная поддержка
(например, расширение для VS Code и языковой сервер с поддержкой подсветки синтаксиса и автоформатирования).

### Формализация теории ∞-категорий

Большая часть статьи Рил и Шульмана (вплоть до леммы Йонеды для ∞-категорий) уже формализована на Rzk (см. [Yoneda for ∞-categories](https://emilyriehl.github.io/yoneda/)[^2]).
Оставшие результаты, а также результаты из других работ находятся в процессе формализации (см. проект по формализации симплициальной гомопотической теории типов, [sHoTT](https://rzk-lang.github.io/sHoTT/)).
Также некоторые усилия направлены на формализацию "книжной"[^6] [^7] гомотопической теории типов (см. [hottbook](https://rzk-lang.github.io/hottbook/)).

### Работа с Rzk

Рекомендуемый способ работы с Rzk — через среду разработки VS Code (см. [Первые шаги](getting-started/install.md)).
Тем не менее, вы также можете скачать исполняемые файлы на [странице релизов на GitHub](https://github.com/rzk-lang/rzk/releases),
собрать самостоятельно [из исходников](getting-started/install.md#install-from-sources),
а также попытаться интегрировать языковой сервер Rzk с вашим любимым текстовым редактором.
Для небольших формализаций, умещающихся в один файл, можно также воспользоваться [онлайн-песочницей Rzk](playground/index.html).

### Реализация

Помимо своей основной цели, Rzk также служит полем экспериментов для методов реализации решателей теорем (на языке Haskell).
В частности, в реализации используется вариант абстрактного синтаксиса второго порядка
для комфортной работы со связанными переменными и, в будущем, для вывода (зависимых) типов
через унификацию высшего порядка[^3] [^4].
В конечном итоге, хочется получить приемлемое общее (библиотечное) решение для унификации высшего порядка и вывода типов,
оставив реализацию Rzk (по крайней мере, его ядра) небольшим, простым для поддержки, и надёжным.

Ещё одна интересная деталь реализации Rzk — это автоматический решатель для слоя форм (tope layer)[^5].
Это встроенный полностью автоматический решатель для варианта интуиционистской логики,
необходимый для проверки типов.
Хотя текущая реализация решателя относительно проста,
её хватает на практике для проверки доказательств о синтетических ∞-категориях
без нужды в дополнительном коде от формализующего математика.

Rzk и сопутствующие инструменты разработаны всего парой человек.
Вы можете ознакомится с участниками проекта в файле [CONTRIBUTORS.md](CONTRIBUTORS.md).

### Обсуждения вокруг Rzk

Для всех желающих доступен (преимущественно англоязычный) чат Zulip,
в котором можно обсудить формализации, разработку Rzk и смежные проекты:

[Присоединиться к сообществу Rzk в Zulip](https://rzk-lang.zulipchat.com/register/){ .md-button .md-button--primary }

[^1]:
    Emily Riehl & Michael Shulman. _A type theory for synthetic ∞-categories_.
    Higher Structures 1(1), 147-224. 2017. <https://arxiv.org/abs/1705.07442>

[^2]:
    Nikolai Kudasov, Emily Riehl, Jonathan Weinberger.
    _Formalizing the ∞-categorical Yoneda lemma_. 2023. <https://arxiv.org/abs/2309.08340>

[^3]:
    Nikolai Kudasov. _Functional Pearl: Dependent type inference via free higher-order unification_. 2022.
    <https://arxiv.org/abs/2204.05653>

[^4]:
    Nikolai Kudasov. _E-Unification for Second-Order Abstract Syntax_. In 8th International Conference on Formal Structures for Computation and Deduction (FSCD 2023). Leibniz International Proceedings in Informatics (LIPIcs), Volume 260, pp. 10:1-10:22, Schloss Dagstuhl - Leibniz-Zentrum für Informatik (2023)
    <https://doi.org/10.4230/LIPIcs.FSCD.2023.10>

[^5]:
    Nikolai Kudasov. Experimental prover for Tope logic. SCAN 2023, pages 37–39. 2023.
    <https://www.mathnet.ru/ConfLogos/2220/abstracts.pdf#page=38>

[^6]:
    The Univalent Foundations Program. _Homotopy Type Theory: Univalent Foundations of Mathematics_.
    Institute for Advanced Study. 2013. <https://homotopytypetheory.org/book/>

[^7]:
    Международный коллектив авторов. _Гомотопическая теория типов: унивалентные основания математики_.
    ИНСТИТУТ ПЕРСПЕКТИВНЫХ ИССЛЕДОВАНИЙ. Перевод и редактирование: ГЕННАДИЙ ЧЕРНЫШЕВ.
    <https://henrychern.files.wordpress.com/2022/10/hott2.pdf>

## Другие решатели для гомотопической теории типов

Rzk — не первый и не единственный решатель для (варианта) гомотопической теории типов.
См. [краткое сравнение Rzk с другими решателями](related.md).
