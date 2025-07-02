# umapyoi - カレンダーAPI to iCal コンバーター

カレンダーAPIからイベントデータを取得し、標準的なiCalendar形式(.icsファイル)に変換するHaskell製ツールです。

## 特徴

- カレンダーAPIからイベントデータを取得
- RFC 5545準拠のiCalendar形式に変換
- 以下のイベント情報をサポート:
  - タイトル、説明文
  - 開始日時/終了日時
  - 終日イベント/複数日イベント
  - 場所情報
  - イベントカラー

## インストール方法

```sh
stack install
```

## 使い方

```sh
umapyoi <API_URL> [output_file]
```

例:
```sh
umapyoi "https://example.com/api/calendar" mycalendar.ics
```
