# Установка Rzk

## Через расширение VS Code (рекомендуется)

Следуйте этим инструкциям, чтобы настроить работу с Rzk в редакторе VS Code.

1. Установите [VS Code](https://code.visualstudio.com/).
2. Запустите VS Code и установите [расширение `rzk`](https://marketplace.visualstudio.com/items?itemName=NikolaiKudasovfizruk.rzk-1-experimental-highlighting).
3. Создайте новый файл через "Файл > Создать текстовый файл" (<kbd>Ctrl+N</kbd>). Нажмите `Select a language`, введите в поиске `rzk` и выберите "Literate Rzk Markdown".
   ![VS Code rzk language selector.](../assets/images/vscode-rzk-select-language.png)
4. Вы должны увидеть следующее сообщение:
   ![VS Code rzk install prompt.](../assets/images/vscode-rzk-install-prompt.png)
5. Нажмите "Yes".
6. Пока Rzk устанавливается, скопируйте и вставьте следующий текст в открытый файл:

   ````markdown
   # Пример литературного кода с Rzk

   ```rzk
   #lang rzk-1

   -- тождественная функция
   #define id (A : U)
     : A -> A
     := \ x -> x
   ```
   ````

7. Когда установка завершится, вы должны увидеть следующее сообщение:
   ![VS Code rzk reload prompt.](../assets/images/vscode-rzk-install-success-reload-prompt.png)
8. Нажмите "Reload" (перезагрузить VS Code).
9. Сохраните ваш файл (например, как `example.rzk.md`).
10. Откройте терминал внутри VS Code (<kbd>Ctrl+`</kbd>).

    <!-- ` -->

11. В терминале запустите команду

    ```sh
    rzk typecheck example.rzk.md
    ```

12. Вы должны увидеть что-то такое:

    ```text
    Loading file example.rzk.md
    Checking module from example.rzk.md
    [ 1 out of 1 ] Checking #define id
    Everything is ok!
    ```

13. Поздравляем! Теперь ваш VS Code настроен на работу с Rzk :) Заметьте, что расширение будет уведомлять вас о наличии обновлений Rzk и предлагать обновить автоматически.

14. Можете перейти к [Быстрому началу](quickstart.rzk.md) чтобы познакомиться с языком Rzk!

## Установка исполняемых файлов

### Через страницу релизов на GitHub

Вы можете скачать исполняемые файлы (для Linux, Windows, и macOS) напрямую со страницы релизов на GitHub: <https://github.com/rzk-lang/rzk/releases>.
Если ваша платформа не поддержана, вы можете попробовать установить Rzk из исходников (см. ниже)
или оставить пожелание о расширении поддержки на странице задач: <https://github.com/rzk-lang/rzk/issues/new>.

## Сборка и установка из исходников

Вы можете установить Rzk из исходников: вы можете либо скачать стабильную версию из репозитория пакетов Hackage, либо собрать самую свежую версию из ветки `develop` на GitHub.

### Stack

Чтобы собрать и установить Rzk при помощи Stack, из репозитория Hackage:

```sh
stack install rzk
```

Чтобы собрать и установить Rzk при помощи Stack, из исходников на GitHub:

```sh
git clone https://github.com/rzk-lang/rzk.git
cd rzk
git checkout develop
stack build && stack install
```

### cabal-install

Чтобы собрать и установить Rzk при помощи `cabal-install`, из репозитория Hackage:

```sh
cabal v2-update
cabal v2-install rzk
```

Чтобы собрать и установить Rzk при помощи `cabal-install`, из исходников на GitHub:

```sh
git clone https://github.com/rzk-lang/rzk.git
cd rzk
git checkout develop
cabal v2-build && cabal v2-install
```

### Nix

!!! warning "Раздел в работе."

    Раздел нуждается в доработке.
